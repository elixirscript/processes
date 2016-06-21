'use strict';

function _interopDefault (ex) { return (ex && (typeof ex === 'object') && 'default' in ex) ? ex['default'] : ex; }

var ErlangTypes = _interopDefault(require('erlang-types'));

/* @flow */

class Mailbox {
  constructor() {
    this.messages = [];
  }

  deliver(message) {
    this.messages.push(message);
    return message;
  }

  get() {
    return this.messages;
  }

  isEmpty() {
    return this.messages.length === 0;
  }

  removeAt(index) {
    this.messages.splice(index, 1);
  }
}

var States = {
  NORMAL: Symbol.for("normal"),
  KILL: Symbol.for("kill"),
  SUSPEND: Symbol.for("suspend"),
  CONTINUE: Symbol.for("continue"),
  RECEIVE: Symbol.for("receive"),
  SEND: Symbol.for("send"),
  SLEEPING: Symbol.for("sleeping"),
  RUNNING: Symbol.for("running"),
  SUSPENDED: Symbol.for("suspended"),
  STOPPED: Symbol.for("stopped"),
  SLEEP: Symbol.for("sleep"),
  EXIT: Symbol.for("exit"),
  NOMATCH: Symbol.for("no_match")
};

function is_sleep(value) {
  return Array.isArray(value) && value[0] === States.SLEEP;
}

function is_receive(value) {
  return Array.isArray(value) && value[0] === States.RECEIVE;
}

function receive_timed_out(value) {
  return value[2] != null && value[2] < Date.now();
}

class Process {
  constructor(pid, func, args, mailbox, system) {
    this.pid = pid;
    this.func = func;
    this.args = args;
    this.mailbox = mailbox;
    this.system = system;
    this.status = States.STOPPED;
    this.dict = {};
    this.flags = {};
    this.monitors = [];
  }

  start() {
    const function_scope = this;
    let machine = this.main();

    this.system.schedule(function () {
      function_scope.system.set_current(function_scope.pid);
      function_scope.run(machine, machine.next());
    }, this.pid);
  }

  *main() {
    let retval = States.NORMAL;

    try {
      yield* this.func.apply(null, this.args);
    } catch (e) {
      console.error(e);
      retval = e;
    }

    this.system.exit(retval);
  }

  process_flag(flag, value) {
    const old_value = this.flags[flag];
    this.flags[flag] = value;
    return old_value;
  }

  is_trapping_exits() {
    return this.flags[Symbol.for("trap_exit")] && this.flags[Symbol.for("trap_exit")] == true;
  }

  signal(reason) {
    if (reason !== States.NORMAL) {
      console.error(reason);
    }

    this.system.remove_proc(this.pid, reason);
  }

  receive(fun) {
    let value = States.NOMATCH;
    let messages = this.mailbox.get();

    for (let i = 0; i < messages.length; i++) {
      try {
        value = fun(messages[i]);
        if (value !== States.NOMATCH) {
          this.mailbox.removeAt(i);
          break;
        }
      } catch (e) {
        if (e.constructor.name != "MatchError") {
          this.exit(e);
        }
      }
    }

    return value;
  }

  run(machine, step) {
    const function_scope = this;

    if (!step.done) {
      let value = step.value;

      if (is_sleep(value)) {

        this.system.delay(function () {
          function_scope.system.set_current(function_scope.pid);
          function_scope.run(machine, machine.next());
        }, value[1]);
      } else if (is_receive(value) && receive_timed_out(value)) {

        let result = value[3]();

        this.system.schedule(function () {
          function_scope.system.set_current(function_scope.pid);
          function_scope.run(machine, machine.next(result));
        });
      } else if (is_receive(value)) {

        let result = function_scope.receive(value[1]);

        if (result === States.NOMATCH) {
          this.system.suspend(function () {
            function_scope.system.set_current(function_scope.pid);
            function_scope.run(machine, step);
          });
        } else {
          this.system.schedule(function () {
            function_scope.system.set_current(function_scope.pid);
            function_scope.run(machine, machine.next(result));
          });
        }
      } else {
        this.system.schedule(function () {
          function_scope.system.set_current(function_scope.pid);
          function_scope.run(machine, machine.next(value));
        });
      }
    }
  }
}

class ProcessQueue {
  constructor(pid) {
    this.pid = pid;
    this.tasks = [];
  }

  empty() {
    return this.tasks.length === 0;
  }

  add(task) {
    this.tasks.push(task);
  }

  next() {
    return this.tasks.shift();
  }
}

class Scheduler {
  constructor(throttle = 0, reductions_per_process = 8) {
    this.isRunning = false;
    this.invokeLater = function (callback) {
      setTimeout(callback, throttle);
    };

    // In our case a reduction is equal to a task call
    // Controls how many tasks are called at a time per process
    this.reductions_per_process = reductions_per_process;
    this.queues = new Map();
    this.run();
  }

  addToQueue(pid, task) {
    if (!this.queues.has(pid)) {
      this.queues.set(pid, new ProcessQueue(pid));
    }

    this.queues.get(pid).add(task);
  }

  removePid(pid) {
    this.isRunning = true;

    this.queues.delete(pid);

    this.isRunning = false;
  }

  run() {
    if (this.isRunning) {
      this.invokeLater(() => {
        this.run();
      });
    } else {
      for (let [pid, queue] of this.queues) {
        let reductions = 0;
        while (queue && !queue.empty() && reductions < this.reductions_per_process) {
          let task = queue.next();
          this.isRunning = true;

          let result;

          try {
            result = task();
          } catch (e) {
            console.error(e);
            result = e;
          }

          this.isRunning = false;

          if (result instanceof Error) {
            throw result;
          }

          reductions++;
        }
      }

      this.invokeLater(() => {
        this.run();
      });
    }
  }

  addToScheduler(pid, task, dueTime = 0) {
    if (dueTime === 0) {
      this.invokeLater(() => {
        this.addToQueue(pid, task);
      });
    } else {
      setTimeout(() => {
        this.addToQueue(pid, task);
      }, dueTime);
    }
  }

  schedule(pid, task) {
    this.addToScheduler(pid, () => {
      task();
    });
  }

  scheduleFuture(pid, dueTime, task) {
    this.addToScheduler(pid, () => {
      task();
    }, dueTime);
  }
}

class ProcessSystem {

  constructor() {
    this.pids = new Map();
    this.mailboxes = new Map();
    this.names = new Map();
    this.links = new Map();
    this.monitors = new Map();

    const throttle = 5; //ms between scheduled tasks
    this.current_process = null;
    this.scheduler = new Scheduler(throttle);
    this.suspended = new Map();

    let process_system_scope = this;
    this.main_process_pid = this.spawn(function* () {
      while (true) {
        yield process_system_scope.sleep(10000);
      }
    });
    this.set_current(this.main_process_pid);
  }

  static *run(fun, args, context = null) {
    if (fun.constructor.name === "GeneratorFunction") {
      return yield* fun.apply(context, args);
    } else {
      return yield fun.apply(context, args);
    }
  }

  spawn(...args) {
    if (args.length === 1) {
      let fun = args[0];
      return this.add_proc(fun, [], false).pid;
    } else {
      let mod = args[0];
      let fun = args[1];
      let the_args = args[2];

      return this.add_proc(mod[fun], the_args, false, false).pid;
    }
  }

  spawn_link(...args) {
    if (args.length === 1) {
      let fun = args[0];
      return this.add_proc(fun, [], true, false).pid;
    } else {
      let mod = args[0];
      let fun = args[1];
      let the_args = args[2];

      return this.add_proc(mod[fun], the_args, true, false).pid;
    }
  }

  link(pid) {
    this.links.get(this.pid()).add(pid);
    this.links.get(pid).add(this.pid());
  }

  unlink(pid) {
    this.links.get(this.pid()).delete(pid);
    this.links.get(pid).delete(this.pid());
  }

  spawn_monitor(...args) {
    if (args.length === 1) {
      let fun = args[0];
      let process = this.add_proc(fun, [], false, true);
      return [process.pid, process.monitors[0]];
    } else {
      let mod = args[0];
      let fun = args[1];
      let the_args = args[2];
      let process = this.add_proc(mod[fun], the_args, false, true);

      return [process.pid, process.monitors[0]];
    }
  }

  monitor(pid) {
    const real_pid = this.pidof(pid);
    const ref = this.make_ref();

    if (real_pid) {

      this.monitors.set(ref, { 'monitor': this.current_process.pid, 'monitee': real_pid });
      this.pids.get(real_pid).monitors(ref);
      return ref;
    } else {
      this.send(this.current_process.pid, ['DOWN', ref, pid, real_pid, Symbol.for('noproc')]);
      return ref;
    }
  }

  demonitor(ref) {
    if (this.monitor.has(ref)) {
      this.monitor.delete(ref);
      return true;
    }

    return false;
  }

  set_current(id) {
    let pid = this.pidof(id);
    if (pid !== null) {
      this.current_process = this.pids.get(pid);
      this.current_process.status = States.RUNNING;
    }
  }

  add_proc(fun, args, linked, monitored) {
    let newpid = new ErlangTypes.PID();
    let mailbox = new Mailbox();
    let newproc = new Process(newpid, fun, args, mailbox, this);

    this.pids.set(newpid, newproc);
    this.mailboxes.set(newpid, mailbox);
    this.links.set(newpid, new Set());

    if (linked) {
      this.link(newpid);
    }

    if (monitored) {
      this.monitor(newpid);
    }

    newproc.start();
    return newproc;
  }

  remove_proc(pid, exitreason) {
    this.pids.delete(pid);
    this.unregister(pid);
    this.scheduler.removePid(pid);

    if (this.links.has(pid)) {
      for (let linkpid of this.links.get(pid)) {
        this.exit(linkpid, exitreason);
        this.links.get(linkpid).delete(pid);
      }

      this.links.delete(pid);
    }
  }

  register(name, pid) {
    if (!this.names.has(name)) {
      this.names.set(name, pid);
    } else {
      throw new Error("Name is already registered to another process");
    }
  }

  whereis(name) {
    return this.names.has(name) ? this.names.get(name) : null;
  }

  registered() {
    return this.names.keys();
  }

  unregister(pid) {
    for (let name of this.names.keys()) {
      if (this.names.has(name) && this.names.get(name) === pid) {
        this.names.delete(name);
      }
    }
  }

  pid() {
    return this.current_process.pid;
  }

  pidof(id) {
    if (id instanceof ErlangTypes.PID) {
      return this.pids.has(id) ? id : null;
    } else if (id instanceof Process) {
      return id.pid;
    } else {
      let pid = this.whereis(id);
      if (pid === null) throw "Process name not registered: " + id + " (" + typeof id + ")";
      return pid;
    }
  }

  send(id, msg) {
    const pid = this.pidof(id);

    if (pid) {
      this.mailboxes.get(pid).deliver(msg);

      if (this.suspended.has(pid)) {
        let fun = this.suspended.get(pid);
        this.suspended.delete(pid);
        this.schedule(fun);
      }
    }

    return msg;
  }

  receive(fun, timeout = 0, timeoutFn = () => true) {
    let DateTimeout = null;

    if (timeout === 0 || timeout === Infinity) {
      DateTimeout = null;
    } else {
      DateTimeout = Date.now() + timeout;
    }

    return [States.RECEIVE, fun, DateTimeout, timeoutFn];
  }

  sleep(duration) {
    return [States.SLEEP, duration];
  }

  suspend(fun) {
    this.current_process.status = States.SUSPENDED;
    this.suspended.set(this.current_process.pid, fun);
  }

  delay(fun, time) {
    this.current_process.status = States.SLEEPING;
    this.scheduler.scheduleFuture(this.current_process.pid, time, fun);
  }

  schedule(fun, pid) {
    const the_pid = pid != null ? pid : this.current_process.pid;
    this.scheduler.schedule(the_pid, fun);
  }

  exit(one, two) {
    let pid = null;
    let reason = null;
    let process = null;

    if (two) {
      let pid = one;
      let reason = two;
      let process = this.pids.get(this.pidof(pid));

      if (process && process.is_trapping_exits() || reason === States.KILL || reason === States.NORMAL) {
        this.mailboxes.get(process.pid).deliver([States.EXIT, this.pid(), reason]);
      } else {
        process.signal(reason);
      }
    } else {
      let pid = this.current_process.pid;
      let reason = one;
      let process = this.current_proces;
      process.signal(reason);
    }

    for (let ref in process.monitors) {
      let mons = this.monitors.get(ref);
      this.send(mons['monitor'], ['DOWN', ref, mons['monitee'], mons['monitee'], reason]);
    }
  }

  error(reason) {
    this.current_process.signal(reason);
  }

  process_flag(...args) {
    if (args.length == 2) {
      const flag = args[0];
      const value = args[1];
      return this.current_process.process_flag(flag, value);
    } else {
      const pid = this.pidof(args[0]);
      const flag = args[1];
      const value = args[2];
      return this.pids.get(pid).process_flag(flag, value);
    }
  }

  put(key, value) {
    this.current_process.dict[key] = value;
  }

  get_process_dict() {
    return this.current_process.dict;
  }

  get(key, default_value = null) {
    if (key in this.current_process.dict) {
      return this.current_process.dict[key];
    } else {
      return default_value;
    }
  }

  get_keys(value) {
    if (value) {
      let keys = [];

      for (let key of Object.keys(this.current_process.dict)) {
        if (this.current_process.dict[key] === value) {
          keys.push(key);
        }
      }

      return keys;
    }

    return Object.keys(this.current_process.dict);
  }

  erase(key) {
    if (key != null) {
      delete this.current_process.dict[key];
    } else {
      this.current_process.dict = {};
    }
  }

  is_alive(pid) {
    const real_pid = this.pidof(pid);
    return real_pid != null;
  }

  list() {
    return this.pids.keys();
  }

  make_ref() {
    return new ErlangTypes.Reference();
  }
}

var index = {
  ProcessSystem
};

module.exports = index;
//# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjpudWxsLCJzb3VyY2VzIjpbIi4uL3NyYy9wcm9jZXNzZXMvbWFpbGJveC5qcyIsIi4uL3NyYy9wcm9jZXNzZXMvc3RhdGVzLmpzIiwiLi4vc3JjL3Byb2Nlc3Nlcy9wcm9jZXNzLmpzIiwiLi4vc3JjL3Byb2Nlc3Nlcy9zY2hlZHVsZXIuanMiLCIuLi9zcmMvcHJvY2Vzc2VzL3Byb2Nlc3Nfc3lzdGVtLmpzIiwiLi4vc3JjL2luZGV4LmpzIl0sInNvdXJjZXNDb250ZW50IjpbIlwidXNlIHN0cmljdFwiO1xuXG4vKiBAZmxvdyAqL1xuXG5jbGFzcyBNYWlsYm94e1xuICBjb25zdHJ1Y3Rvcigpe1xuICAgIHRoaXMubWVzc2FnZXMgPSBbXTtcbiAgfVxuXG4gIGRlbGl2ZXIobWVzc2FnZSl7XG4gICAgdGhpcy5tZXNzYWdlcy5wdXNoKG1lc3NhZ2UpO1xuICAgIHJldHVybiBtZXNzYWdlO1xuICB9XG5cbiAgZ2V0KCl7XG4gICAgcmV0dXJuIHRoaXMubWVzc2FnZXM7XG4gIH1cblxuICBpc0VtcHR5KCl7XG4gICAgcmV0dXJuIHRoaXMubWVzc2FnZXMubGVuZ3RoID09PSAwO1xuICB9XG5cbiAgcmVtb3ZlQXQoaW5kZXgpe1xuICAgIHRoaXMubWVzc2FnZXMuc3BsaWNlKGluZGV4LCAxKTtcbiAgfVxufVxuXG5leHBvcnQgZGVmYXVsdCBNYWlsYm94O1xuIiwiZXhwb3J0IGRlZmF1bHQge1xuICBOT1JNQUw6IFN5bWJvbC5mb3IoXCJub3JtYWxcIiksXG4gIEtJTEw6IFN5bWJvbC5mb3IoXCJraWxsXCIpLFxuICBTVVNQRU5EOiBTeW1ib2wuZm9yKFwic3VzcGVuZFwiKSxcbiAgQ09OVElOVUU6IFN5bWJvbC5mb3IoXCJjb250aW51ZVwiKSxcbiAgUkVDRUlWRTogU3ltYm9sLmZvcihcInJlY2VpdmVcIiksXG4gIFNFTkQ6IFN5bWJvbC5mb3IoXCJzZW5kXCIpLFxuICBTTEVFUElORzogU3ltYm9sLmZvcihcInNsZWVwaW5nXCIpLFxuICBSVU5OSU5HOiBTeW1ib2wuZm9yKFwicnVubmluZ1wiKSxcbiAgU1VTUEVOREVEOiBTeW1ib2wuZm9yKFwic3VzcGVuZGVkXCIpLFxuICBTVE9QUEVEOiBTeW1ib2wuZm9yKFwic3RvcHBlZFwiKSxcbiAgU0xFRVA6IFN5bWJvbC5mb3IoXCJzbGVlcFwiKSxcbiAgRVhJVDogU3ltYm9sLmZvcihcImV4aXRcIiksXG4gIE5PTUFUQ0g6IFN5bWJvbC5mb3IoXCJub19tYXRjaFwiKVxufSIsIlwidXNlIHN0cmljdFwiO1xuXG4vKiBAZmxvdyAqL1xuaW1wb3J0IE1haWxib3ggZnJvbSBcIi4vbWFpbGJveFwiO1xuaW1wb3J0IFByb2Nlc3NTeXN0ZW0gZnJvbSBcIi4vcHJvY2Vzc19zeXN0ZW1cIjtcbmltcG9ydCBTdGF0ZXMgZnJvbSBcIi4vc3RhdGVzXCI7XG5cbmZ1bmN0aW9uIGlzX3NsZWVwKHZhbHVlKXtcbiAgcmV0dXJuIEFycmF5LmlzQXJyYXkodmFsdWUpICYmIHZhbHVlWzBdID09PSBTdGF0ZXMuU0xFRVA7XG59XG5cbmZ1bmN0aW9uIGlzX3JlY2VpdmUodmFsdWUpe1xuICByZXR1cm4gQXJyYXkuaXNBcnJheSh2YWx1ZSkgJiYgdmFsdWVbMF0gPT09IFN0YXRlcy5SRUNFSVZFO1xufVxuXG5mdW5jdGlvbiByZWNlaXZlX3RpbWVkX291dCh2YWx1ZSl7XG4gIHJldHVybiB2YWx1ZVsyXSAhPSBudWxsICYmIHZhbHVlWzJdIDwgRGF0ZS5ub3coKTtcbn1cblxuY2xhc3MgUHJvY2VzcyB7XG4gIGNvbnN0cnVjdG9yKHBpZCwgZnVuYywgYXJncywgbWFpbGJveCwgc3lzdGVtKXtcbiAgICB0aGlzLnBpZCA9IHBpZDtcbiAgICB0aGlzLmZ1bmMgPSBmdW5jO1xuICAgIHRoaXMuYXJncyA9IGFyZ3M7XG4gICAgdGhpcy5tYWlsYm94ID0gbWFpbGJveDtcbiAgICB0aGlzLnN5c3RlbSA9IHN5c3RlbTtcbiAgICB0aGlzLnN0YXR1cyA9IFN0YXRlcy5TVE9QUEVEO1xuICAgIHRoaXMuZGljdCA9IHt9O1xuICAgIHRoaXMuZmxhZ3MgPSB7fTtcbiAgICB0aGlzLm1vbml0b3JzID0gW107XG4gIH1cblxuICBzdGFydCgpe1xuICAgIGNvbnN0IGZ1bmN0aW9uX3Njb3BlID0gdGhpcztcbiAgICBsZXQgbWFjaGluZSA9IHRoaXMubWFpbigpO1xuXG4gICAgdGhpcy5zeXN0ZW0uc2NoZWR1bGUoZnVuY3Rpb24oKSB7XG4gICAgICBmdW5jdGlvbl9zY29wZS5zeXN0ZW0uc2V0X2N1cnJlbnQoZnVuY3Rpb25fc2NvcGUucGlkKTtcbiAgICAgIGZ1bmN0aW9uX3Njb3BlLnJ1bihtYWNoaW5lLCBtYWNoaW5lLm5leHQoKSk7XG4gICAgfSwgdGhpcy5waWQpO1xuICB9XG5cbiAgKm1haW4oKSB7XG4gICAgbGV0IHJldHZhbCA9IFN0YXRlcy5OT1JNQUw7XG5cbiAgICB0cnkge1xuICAgICAgeWllbGQqIHRoaXMuZnVuYy5hcHBseShudWxsLCB0aGlzLmFyZ3MpO1xuICAgIH0gY2F0Y2goZSkge1xuICAgICAgY29uc29sZS5lcnJvcihlKTtcbiAgICAgIHJldHZhbCA9IGU7XG4gICAgfVxuXG4gICAgdGhpcy5zeXN0ZW0uZXhpdChyZXR2YWwpO1xuICB9XG5cbiAgcHJvY2Vzc19mbGFnKGZsYWcsIHZhbHVlKXtcbiAgICBjb25zdCBvbGRfdmFsdWUgPSB0aGlzLmZsYWdzW2ZsYWddO1xuICAgIHRoaXMuZmxhZ3NbZmxhZ10gPSB2YWx1ZTtcbiAgICByZXR1cm4gb2xkX3ZhbHVlO1xuICB9XG5cbiAgaXNfdHJhcHBpbmdfZXhpdHMoKXtcbiAgICByZXR1cm4gdGhpcy5mbGFnc1tTeW1ib2wuZm9yKFwidHJhcF9leGl0XCIpXSAmJiB0aGlzLmZsYWdzW1N5bWJvbC5mb3IoXCJ0cmFwX2V4aXRcIildID09IHRydWU7XG4gIH1cblxuICBzaWduYWwocmVhc29uKXtcbiAgICBpZihyZWFzb24gIT09IFN0YXRlcy5OT1JNQUwpe1xuICAgICAgY29uc29sZS5lcnJvcihyZWFzb24pO1xuICAgIH1cblxuICAgIHRoaXMuc3lzdGVtLnJlbW92ZV9wcm9jKHRoaXMucGlkLCByZWFzb24pO1xuICB9XG5cbiAgcmVjZWl2ZShmdW4pe1xuICAgIGxldCB2YWx1ZSA9IFN0YXRlcy5OT01BVENIO1xuICAgIGxldCBtZXNzYWdlcyA9IHRoaXMubWFpbGJveC5nZXQoKTtcblxuICAgIGZvcihsZXQgaSA9IDA7IGkgPCBtZXNzYWdlcy5sZW5ndGg7IGkrKyl7XG4gICAgICB0cnl7XG4gICAgICAgIHZhbHVlID0gZnVuKG1lc3NhZ2VzW2ldKTtcbiAgICAgICAgaWYodmFsdWUgIT09IFN0YXRlcy5OT01BVENIKXtcbiAgICAgICAgICB0aGlzLm1haWxib3gucmVtb3ZlQXQoaSk7XG4gICAgICAgICAgYnJlYWs7XG4gICAgICAgIH1cbiAgICAgIH1jYXRjaChlKXtcbiAgICAgICAgaWYoZS5jb25zdHJ1Y3Rvci5uYW1lICE9IFwiTWF0Y2hFcnJvclwiKXtcbiAgICAgICAgICB0aGlzLmV4aXQoZSk7XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICB9XG5cbiAgICByZXR1cm4gdmFsdWU7XG4gIH1cblxuICBydW4obWFjaGluZSwgc3RlcCl7XG4gICAgY29uc3QgZnVuY3Rpb25fc2NvcGUgPSB0aGlzO1xuXG4gICAgaWYoIXN0ZXAuZG9uZSl7XG4gICAgICBsZXQgdmFsdWUgPSBzdGVwLnZhbHVlO1xuXG4gICAgICBpZihpc19zbGVlcCh2YWx1ZSkpe1xuXG4gICAgICAgIHRoaXMuc3lzdGVtLmRlbGF5KGZ1bmN0aW9uKCkge1xuICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnN5c3RlbS5zZXRfY3VycmVudChmdW5jdGlvbl9zY29wZS5waWQpO1xuICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnJ1bihtYWNoaW5lLCBtYWNoaW5lLm5leHQoKSk7XG4gICAgICAgIH0sIHZhbHVlWzFdKTtcblxuICAgICAgfWVsc2UgaWYoaXNfcmVjZWl2ZSh2YWx1ZSkgJiYgcmVjZWl2ZV90aW1lZF9vdXQodmFsdWUpKXtcblxuICAgICAgICBsZXQgcmVzdWx0ID0gdmFsdWVbM10oKTtcblxuICAgICAgICB0aGlzLnN5c3RlbS5zY2hlZHVsZShmdW5jdGlvbigpIHtcbiAgICAgICAgICBmdW5jdGlvbl9zY29wZS5zeXN0ZW0uc2V0X2N1cnJlbnQoZnVuY3Rpb25fc2NvcGUucGlkKTtcbiAgICAgICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgbWFjaGluZS5uZXh0KHJlc3VsdCkpO1xuICAgICAgICB9KTtcblxuICAgICAgfWVsc2UgaWYoaXNfcmVjZWl2ZSh2YWx1ZSkpe1xuXG4gICAgICAgIGxldCByZXN1bHQgPSBmdW5jdGlvbl9zY29wZS5yZWNlaXZlKHZhbHVlWzFdKTtcblxuICAgICAgICBpZihyZXN1bHQgPT09IFN0YXRlcy5OT01BVENIKXtcbiAgICAgICAgICB0aGlzLnN5c3RlbS5zdXNwZW5kKGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICAgICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgc3RlcCk7XG4gICAgICAgICAgfSk7XG4gICAgICAgIH1lbHNle1xuICAgICAgICAgIHRoaXMuc3lzdGVtLnNjaGVkdWxlKGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICAgICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgbWFjaGluZS5uZXh0KHJlc3VsdCkpO1xuICAgICAgICAgIH0pO1xuICAgICAgICB9XG5cbiAgICAgIH1lbHNle1xuICAgICAgICB0aGlzLnN5c3RlbS5zY2hlZHVsZShmdW5jdGlvbigpIHtcbiAgICAgICAgICBmdW5jdGlvbl9zY29wZS5zeXN0ZW0uc2V0X2N1cnJlbnQoZnVuY3Rpb25fc2NvcGUucGlkKTtcbiAgICAgICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgbWFjaGluZS5uZXh0KHZhbHVlKSk7XG4gICAgICAgIH0pO1xuICAgICAgfVxuICAgIH1cbiAgfVxufVxuXG5leHBvcnQgZGVmYXVsdCBQcm9jZXNzO1xuIiwiXCJ1c2Ugc3RyaWN0XCI7XG5cbmNsYXNzIFByb2Nlc3NRdWV1ZSB7XG4gIGNvbnN0cnVjdG9yKHBpZCl7XG4gICAgdGhpcy5waWQgPSBwaWQ7XG4gICAgdGhpcy50YXNrcyA9IFtdO1xuICB9XG5cbiAgZW1wdHkoKXtcbiAgICByZXR1cm4gdGhpcy50YXNrcy5sZW5ndGggPT09IDA7XG4gIH1cblxuICBhZGQodGFzayl7XG4gICAgdGhpcy50YXNrcy5wdXNoKHRhc2spO1xuICB9XG5cbiAgbmV4dCgpe1xuICAgIHJldHVybiB0aGlzLnRhc2tzLnNoaWZ0KCk7XG4gIH1cbn1cblxuY2xhc3MgU2NoZWR1bGVyIHtcbiAgICBjb25zdHJ1Y3Rvcih0aHJvdHRsZSA9IDAsIHJlZHVjdGlvbnNfcGVyX3Byb2Nlc3MgPSA4KXtcbiAgICAgICAgdGhpcy5pc1J1bm5pbmcgPSBmYWxzZTtcbiAgICAgICAgdGhpcy5pbnZva2VMYXRlciA9IGZ1bmN0aW9uIChjYWxsYmFjaykgeyBzZXRUaW1lb3V0KGNhbGxiYWNrLCB0aHJvdHRsZSk7IH07XG5cbiAgICAgICAgLy8gSW4gb3VyIGNhc2UgYSByZWR1Y3Rpb24gaXMgZXF1YWwgdG8gYSB0YXNrIGNhbGxcbiAgICAgICAgLy8gQ29udHJvbHMgaG93IG1hbnkgdGFza3MgYXJlIGNhbGxlZCBhdCBhIHRpbWUgcGVyIHByb2Nlc3NcbiAgICAgICAgdGhpcy5yZWR1Y3Rpb25zX3Blcl9wcm9jZXNzID0gcmVkdWN0aW9uc19wZXJfcHJvY2VzcztcbiAgICAgICAgdGhpcy5xdWV1ZXMgPSBuZXcgTWFwKCk7XG4gICAgICAgIHRoaXMucnVuKCk7XG4gIH1cblxuICBhZGRUb1F1ZXVlKHBpZCwgdGFzayl7XG4gICAgaWYoIXRoaXMucXVldWVzLmhhcyhwaWQpKXtcbiAgICAgIHRoaXMucXVldWVzLnNldChwaWQsIG5ldyBQcm9jZXNzUXVldWUocGlkKSk7XG4gICAgfVxuXG4gICAgdGhpcy5xdWV1ZXMuZ2V0KHBpZCkuYWRkKHRhc2spO1xuICB9XG5cbiAgcmVtb3ZlUGlkKHBpZCl7XG4gICAgdGhpcy5pc1J1bm5pbmcgPSB0cnVlO1xuXG4gICAgdGhpcy5xdWV1ZXMuZGVsZXRlKHBpZCk7XG5cbiAgICB0aGlzLmlzUnVubmluZyA9IGZhbHNlO1xuICB9XG5cbiAgcnVuKCl7XG4gICAgaWYgKHRoaXMuaXNSdW5uaW5nKSB7XG4gICAgICB0aGlzLmludm9rZUxhdGVyKCgpID0+IHsgdGhpcy5ydW4oKTsgfSk7XG4gICAgfSBlbHNlIHtcbiAgICAgIGZvcihsZXQgW3BpZCwgcXVldWVdIG9mIHRoaXMucXVldWVzKXtcbiAgICAgICAgbGV0IHJlZHVjdGlvbnMgPSAwO1xuICAgICAgICB3aGlsZShxdWV1ZSAmJiAhcXVldWUuZW1wdHkoKSAmJiByZWR1Y3Rpb25zIDwgdGhpcy5yZWR1Y3Rpb25zX3Blcl9wcm9jZXNzKXtcbiAgICAgICAgICBsZXQgdGFzayA9IHF1ZXVlLm5leHQoKTtcbiAgICAgICAgICB0aGlzLmlzUnVubmluZyA9IHRydWU7XG5cbiAgICAgICAgICBsZXQgcmVzdWx0O1xuXG4gICAgICAgICAgdHJ5e1xuICAgICAgICAgICAgcmVzdWx0ID0gdGFzaygpO1xuICAgICAgICAgIH1jYXRjaChlKXtcbiAgICAgICAgICAgIGNvbnNvbGUuZXJyb3IoZSk7XG4gICAgICAgICAgICByZXN1bHQgPSBlO1xuICAgICAgICAgIH1cblxuICAgICAgICAgIHRoaXMuaXNSdW5uaW5nID0gZmFsc2U7XG5cbiAgICAgICAgICBpZiAocmVzdWx0IGluc3RhbmNlb2YgRXJyb3IpIHtcbiAgICAgICAgICAgIHRocm93IHJlc3VsdDtcbiAgICAgICAgICB9XG5cbiAgICAgICAgICByZWR1Y3Rpb25zKys7XG4gICAgICAgIH1cbiAgICAgIH1cblxuICAgICAgdGhpcy5pbnZva2VMYXRlcigoKSA9PiB7IHRoaXMucnVuKCk7IH0pO1xuICAgIH1cbiAgfVxuXG4gIGFkZFRvU2NoZWR1bGVyKHBpZCwgdGFzaywgZHVlVGltZSA9IDApIHtcbiAgICBpZihkdWVUaW1lID09PSAwKXtcbiAgICAgIHRoaXMuaW52b2tlTGF0ZXIoKCkgPT4ge1xuICAgICAgICB0aGlzLmFkZFRvUXVldWUocGlkLCB0YXNrKTtcbiAgICAgIH0pO1xuICAgIH1lbHNle1xuICAgICAgc2V0VGltZW91dCgoKSA9PiB7XG4gICAgICAgIHRoaXMuYWRkVG9RdWV1ZShwaWQsIHRhc2spO1xuICAgICAgfSwgZHVlVGltZSk7XG4gICAgfVxuICB9O1xuXG4gIHNjaGVkdWxlKHBpZCwgdGFzayl7XG4gICAgdGhpcy5hZGRUb1NjaGVkdWxlcihwaWQsICgpID0+IHsgdGFzaygpOyB9KTtcbiAgfVxuXG4gIHNjaGVkdWxlRnV0dXJlKHBpZCwgZHVlVGltZSwgdGFzayl7XG4gICAgdGhpcy5hZGRUb1NjaGVkdWxlcihwaWQsICgpID0+IHsgdGFzaygpOyB9LCBkdWVUaW1lKTtcbiAgfVxufVxuXG5leHBvcnQgZGVmYXVsdCBTY2hlZHVsZXI7XG4iLCIvKiBAZmxvdyAqL1xuXCJ1c2Ugc3RyaWN0XCI7XG5cbmltcG9ydCBNYWlsYm94IGZyb20gXCIuL21haWxib3hcIjtcbmltcG9ydCBQcm9jZXNzIGZyb20gXCIuL3Byb2Nlc3NcIjtcbmltcG9ydCBTdGF0ZXMgZnJvbSBcIi4vc3RhdGVzXCI7XG5pbXBvcnQgU2NoZWR1bGVyIGZyb20gXCIuL3NjaGVkdWxlclwiO1xuaW1wb3J0IEVybGFuZ1R5cGVzIGZyb20gXCJlcmxhbmctdHlwZXNcIjtcblxuXG5jbGFzcyBQcm9jZXNzU3lzdGVtIHtcblxuICBjb25zdHJ1Y3Rvcigpe1xuICAgIHRoaXMucGlkcyA9IG5ldyBNYXAoKTtcbiAgICB0aGlzLm1haWxib3hlcyA9IG5ldyBNYXAoKTtcbiAgICB0aGlzLm5hbWVzID0gbmV3IE1hcCgpO1xuICAgIHRoaXMubGlua3MgPSBuZXcgTWFwKCk7XG4gICAgdGhpcy5tb25pdG9ycyA9IG5ldyBNYXAoKTtcblxuICAgIGNvbnN0IHRocm90dGxlID0gNTsgLy9tcyBiZXR3ZWVuIHNjaGVkdWxlZCB0YXNrc1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzID0gbnVsbDtcbiAgICB0aGlzLnNjaGVkdWxlciA9IG5ldyBTY2hlZHVsZXIodGhyb3R0bGUpO1xuICAgIHRoaXMuc3VzcGVuZGVkID0gbmV3IE1hcCgpO1xuXG4gICAgbGV0IHByb2Nlc3Nfc3lzdGVtX3Njb3BlID0gdGhpcztcbiAgICB0aGlzLm1haW5fcHJvY2Vzc19waWQgPSB0aGlzLnNwYXduKGZ1bmN0aW9uKigpe1xuICAgICAgICB3aGlsZSh0cnVlKXtcbiAgICAgICAgICB5aWVsZCBwcm9jZXNzX3N5c3RlbV9zY29wZS5zbGVlcCgxMDAwMCk7XG4gICAgICAgIH1cbiAgICB9KTtcbiAgICB0aGlzLnNldF9jdXJyZW50KHRoaXMubWFpbl9wcm9jZXNzX3BpZCk7XG4gIH1cblxuICBzdGF0aWMgKiBydW4oZnVuLCBhcmdzLCBjb250ZXh0ID0gbnVsbCl7XG4gICAgaWYoZnVuLmNvbnN0cnVjdG9yLm5hbWUgPT09IFwiR2VuZXJhdG9yRnVuY3Rpb25cIil7XG4gICAgICByZXR1cm4geWllbGQqIGZ1bi5hcHBseShjb250ZXh0LCBhcmdzKTtcbiAgICB9ZWxzZXtcbiAgICAgIHJldHVybiB5aWVsZCBmdW4uYXBwbHkoY29udGV4dCwgYXJncyk7XG4gICAgfVxuICB9XG5cbiAgc3Bhd24oLi4uYXJncyl7XG4gICAgaWYoYXJncy5sZW5ndGggPT09IDEpe1xuICAgICAgbGV0IGZ1biA9IGFyZ3NbMF07XG4gICAgICByZXR1cm4gdGhpcy5hZGRfcHJvYyhmdW4sIFtdLCBmYWxzZSkucGlkO1xuXG4gICAgfWVsc2V7XG4gICAgICBsZXQgbW9kID0gYXJnc1swXTtcbiAgICAgIGxldCBmdW4gPSBhcmdzWzFdO1xuICAgICAgbGV0IHRoZV9hcmdzID0gYXJnc1syXTtcblxuICAgICAgcmV0dXJuIHRoaXMuYWRkX3Byb2MobW9kW2Z1bl0sIHRoZV9hcmdzLCBmYWxzZSwgZmFsc2UpLnBpZDtcbiAgICB9XG4gIH1cblxuICBzcGF3bl9saW5rKC4uLmFyZ3Mpe1xuICAgIGlmKGFyZ3MubGVuZ3RoID09PSAxKXtcbiAgICAgIGxldCBmdW4gPSBhcmdzWzBdO1xuICAgICAgcmV0dXJuIHRoaXMuYWRkX3Byb2MoZnVuLCBbXSwgdHJ1ZSwgZmFsc2UpLnBpZDtcblxuICAgIH1lbHNle1xuICAgICAgbGV0IG1vZCA9IGFyZ3NbMF07XG4gICAgICBsZXQgZnVuID0gYXJnc1sxXTtcbiAgICAgIGxldCB0aGVfYXJncyA9IGFyZ3NbMl07XG5cbiAgICAgIHJldHVybiB0aGlzLmFkZF9wcm9jKG1vZFtmdW5dLCB0aGVfYXJncywgdHJ1ZSwgZmFsc2UpLnBpZDtcbiAgICB9XG4gIH1cblxuICBsaW5rKHBpZCl7XG4gICAgdGhpcy5saW5rcy5nZXQodGhpcy5waWQoKSkuYWRkKHBpZCk7XG4gICAgdGhpcy5saW5rcy5nZXQocGlkKS5hZGQodGhpcy5waWQoKSk7XG4gIH1cblxuICB1bmxpbmsocGlkKXtcbiAgICB0aGlzLmxpbmtzLmdldCh0aGlzLnBpZCgpKS5kZWxldGUocGlkKTtcbiAgICB0aGlzLmxpbmtzLmdldChwaWQpLmRlbGV0ZSh0aGlzLnBpZCgpKTtcbiAgfVxuXG4gIHNwYXduX21vbml0b3IoLi4uYXJncyl7XG4gICAgaWYoYXJncy5sZW5ndGggPT09IDEpe1xuICAgICAgbGV0IGZ1biA9IGFyZ3NbMF07XG4gICAgICBsZXQgcHJvY2VzcyA9IHRoaXMuYWRkX3Byb2MoZnVuLCBbXSwgZmFsc2UsIHRydWUpO1xuICAgICAgcmV0dXJuIFtwcm9jZXNzLnBpZCwgcHJvY2Vzcy5tb25pdG9yc1swXV07XG5cbiAgICB9ZWxzZXtcbiAgICAgIGxldCBtb2QgPSBhcmdzWzBdO1xuICAgICAgbGV0IGZ1biA9IGFyZ3NbMV07XG4gICAgICBsZXQgdGhlX2FyZ3MgPSBhcmdzWzJdO1xuICAgICAgbGV0IHByb2Nlc3MgPSB0aGlzLmFkZF9wcm9jKG1vZFtmdW5dLCB0aGVfYXJncywgZmFsc2UsIHRydWUpO1xuXG4gICAgICByZXR1cm4gW3Byb2Nlc3MucGlkLCBwcm9jZXNzLm1vbml0b3JzWzBdXTtcbiAgICB9XG4gIH1cblxuICBtb25pdG9yKHBpZCl7XG4gICAgY29uc3QgcmVhbF9waWQgPSB0aGlzLnBpZG9mKHBpZCk7XG4gICAgY29uc3QgcmVmID0gdGhpcy5tYWtlX3JlZigpO1xuXG4gICAgaWYocmVhbF9waWQpe1xuXG4gICAgICB0aGlzLm1vbml0b3JzLnNldChyZWYsIHsnbW9uaXRvcic6IHRoaXMuY3VycmVudF9wcm9jZXNzLnBpZCwgJ21vbml0ZWUnOiByZWFsX3BpZH0pO1xuICAgICAgdGhpcy5waWRzLmdldChyZWFsX3BpZCkubW9uaXRvcnMocmVmKTtcbiAgICAgIHJldHVybiByZWY7XG4gICAgfWVsc2V7XG4gICAgICB0aGlzLnNlbmQodGhpcy5jdXJyZW50X3Byb2Nlc3MucGlkLCBbJ0RPV04nLCByZWYsIHBpZCwgcmVhbF9waWQsIFN5bWJvbC5mb3IoJ25vcHJvYycpXSk7XG4gICAgICByZXR1cm4gcmVmO1xuICAgIH1cbiAgfVxuXG4gIGRlbW9uaXRvcihyZWYpe1xuICAgIGlmKHRoaXMubW9uaXRvci5oYXMocmVmKSl7XG4gICAgICB0aGlzLm1vbml0b3IuZGVsZXRlKHJlZik7XG4gICAgICByZXR1cm4gdHJ1ZTtcbiAgICB9XG5cbiAgICByZXR1cm4gZmFsc2U7XG4gIH1cblxuICBzZXRfY3VycmVudChpZCl7XG4gICAgbGV0IHBpZCA9IHRoaXMucGlkb2YoaWQpO1xuICAgIGlmKHBpZCAhPT0gbnVsbCl7XG4gICAgICB0aGlzLmN1cnJlbnRfcHJvY2VzcyA9IHRoaXMucGlkcy5nZXQocGlkKTtcbiAgICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLnN0YXR1cyA9IFN0YXRlcy5SVU5OSU5HO1xuICAgIH1cbiAgfVxuXG4gIGFkZF9wcm9jKGZ1biwgYXJncywgbGlua2VkLCBtb25pdG9yZWQpe1xuICAgIGxldCBuZXdwaWQgPSBuZXcgRXJsYW5nVHlwZXMuUElEKCk7XG4gICAgbGV0IG1haWxib3ggPSBuZXcgTWFpbGJveCgpO1xuICAgIGxldCBuZXdwcm9jID0gbmV3IFByb2Nlc3MobmV3cGlkLCBmdW4sIGFyZ3MsIG1haWxib3gsIHRoaXMpO1xuXG4gICAgdGhpcy5waWRzLnNldChuZXdwaWQsIG5ld3Byb2MpO1xuICAgIHRoaXMubWFpbGJveGVzLnNldChuZXdwaWQsIG1haWxib3gpO1xuICAgIHRoaXMubGlua3Muc2V0KG5ld3BpZCwgbmV3IFNldCgpKTtcblxuICAgIGlmKGxpbmtlZCl7XG4gICAgICB0aGlzLmxpbmsobmV3cGlkKTtcbiAgICB9XG5cbiAgICBpZihtb25pdG9yZWQpe1xuICAgICAgdGhpcy5tb25pdG9yKG5ld3BpZCk7XG4gICAgfVxuXG4gICAgbmV3cHJvYy5zdGFydCgpO1xuICAgIHJldHVybiBuZXdwcm9jO1xuICB9XG5cbiAgcmVtb3ZlX3Byb2MocGlkLCBleGl0cmVhc29uKXtcbiAgICB0aGlzLnBpZHMuZGVsZXRlKHBpZCk7XG4gICAgdGhpcy51bnJlZ2lzdGVyKHBpZCk7XG4gICAgdGhpcy5zY2hlZHVsZXIucmVtb3ZlUGlkKHBpZCk7XG5cbiAgICBpZih0aGlzLmxpbmtzLmhhcyhwaWQpKXtcbiAgICAgIGZvciAobGV0IGxpbmtwaWQgb2YgdGhpcy5saW5rcy5nZXQocGlkKSkge1xuICAgICAgICB0aGlzLmV4aXQobGlua3BpZCwgZXhpdHJlYXNvbik7XG4gICAgICAgIHRoaXMubGlua3MuZ2V0KGxpbmtwaWQpLmRlbGV0ZShwaWQpO1xuICAgICAgfVxuXG4gICAgICB0aGlzLmxpbmtzLmRlbGV0ZShwaWQpO1xuICAgIH1cbiAgfVxuXG4gIHJlZ2lzdGVyKG5hbWUsIHBpZCl7XG4gICAgaWYoIXRoaXMubmFtZXMuaGFzKG5hbWUpKXtcbiAgICAgIHRoaXMubmFtZXMuc2V0KG5hbWUsIHBpZCk7XG4gICAgfWVsc2V7XG4gICAgICB0aHJvdyBuZXcgRXJyb3IoXCJOYW1lIGlzIGFscmVhZHkgcmVnaXN0ZXJlZCB0byBhbm90aGVyIHByb2Nlc3NcIik7XG4gICAgfVxuICB9XG5cbiAgd2hlcmVpcyhuYW1lKXtcbiAgICByZXR1cm4gdGhpcy5uYW1lcy5oYXMobmFtZSkgPyB0aGlzLm5hbWVzLmdldChuYW1lKSA6IG51bGw7XG4gIH1cblxuICByZWdpc3RlcmVkKCl7XG4gICAgcmV0dXJuIHRoaXMubmFtZXMua2V5cygpO1xuICB9XG5cbiAgdW5yZWdpc3RlcihwaWQpe1xuICAgIGZvcihsZXQgbmFtZSBvZiB0aGlzLm5hbWVzLmtleXMoKSl7XG4gICAgICBpZih0aGlzLm5hbWVzLmhhcyhuYW1lKSAmJiB0aGlzLm5hbWVzLmdldChuYW1lKSA9PT0gcGlkKXtcbiAgICAgICAgdGhpcy5uYW1lcy5kZWxldGUobmFtZSk7XG4gICAgICB9XG4gICAgfVxuICB9XG5cbiAgcGlkKCl7XG4gICAgcmV0dXJuIHRoaXMuY3VycmVudF9wcm9jZXNzLnBpZDtcbiAgfVxuXG4gIHBpZG9mKGlkKXtcbiAgICBpZiAoaWQgaW5zdGFuY2VvZiBFcmxhbmdUeXBlcy5QSUQpIHtcbiAgICAgICByZXR1cm4gdGhpcy5waWRzLmhhcyhpZCkgPyBpZCA6IG51bGw7XG4gICAgfSBlbHNlIGlmIChpZCBpbnN0YW5jZW9mIFByb2Nlc3MpIHtcbiAgICAgICByZXR1cm4gaWQucGlkO1xuICAgIH0gZWxzZSB7XG4gICAgICAgbGV0IHBpZCA9IHRoaXMud2hlcmVpcyhpZCk7XG4gICAgICAgaWYgKHBpZCA9PT0gbnVsbClcbiAgICAgICAgICB0aHJvdyhcIlByb2Nlc3MgbmFtZSBub3QgcmVnaXN0ZXJlZDogXCIgKyBpZCArIFwiIChcIiArIHR5cGVvZihpZCkgKyBcIilcIik7XG4gICAgICAgcmV0dXJuIHBpZDtcbiAgICB9XG4gIH1cblxuICBzZW5kKGlkLCBtc2cpIHtcbiAgICBjb25zdCBwaWQgPSB0aGlzLnBpZG9mKGlkKTtcblxuICAgIGlmKHBpZCl7XG4gICAgICB0aGlzLm1haWxib3hlcy5nZXQocGlkKS5kZWxpdmVyKG1zZyk7XG5cbiAgICAgIGlmKHRoaXMuc3VzcGVuZGVkLmhhcyhwaWQpKXtcbiAgICAgICAgbGV0IGZ1biA9IHRoaXMuc3VzcGVuZGVkLmdldChwaWQpO1xuICAgICAgICB0aGlzLnN1c3BlbmRlZC5kZWxldGUocGlkKTtcbiAgICAgICAgdGhpcy5zY2hlZHVsZShmdW4pO1xuICAgICAgfVxuICAgIH1cblxuICAgIHJldHVybiBtc2c7XG4gIH1cblxuICByZWNlaXZlKGZ1biwgdGltZW91dCA9IDAsIHRpbWVvdXRGbiA9ICgpID0+IHRydWUgKSB7XG4gICAgbGV0IERhdGVUaW1lb3V0ID0gbnVsbDtcblxuICAgIGlmKHRpbWVvdXQgPT09IDAgfHwgdGltZW91dCA9PT0gSW5maW5pdHkpe1xuICAgICAgRGF0ZVRpbWVvdXQgPSBudWxsO1xuICAgIH1lbHNle1xuICAgICAgRGF0ZVRpbWVvdXQgPSBEYXRlLm5vdygpICsgdGltZW91dDtcbiAgICB9XG5cbiAgICByZXR1cm4gW1xuICAgICAgU3RhdGVzLlJFQ0VJVkUsXG4gICAgICBmdW4sXG4gICAgICBEYXRlVGltZW91dCxcbiAgICAgIHRpbWVvdXRGblxuICAgIF07XG4gIH1cblxuICBzbGVlcChkdXJhdGlvbil7XG4gICAgcmV0dXJuIFtTdGF0ZXMuU0xFRVAsIGR1cmF0aW9uXTtcbiAgfVxuXG4gIHN1c3BlbmQoZnVuKXtcbiAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5zdGF0dXMgPSBTdGF0ZXMuU1VTUEVOREVEO1xuICAgIHRoaXMuc3VzcGVuZGVkLnNldCh0aGlzLmN1cnJlbnRfcHJvY2Vzcy5waWQsIGZ1bik7XG4gIH1cblxuICBkZWxheShmdW4sIHRpbWUpe1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLnN0YXR1cyA9IFN0YXRlcy5TTEVFUElORztcbiAgICB0aGlzLnNjaGVkdWxlci5zY2hlZHVsZUZ1dHVyZSh0aGlzLmN1cnJlbnRfcHJvY2Vzcy5waWQsIHRpbWUsIGZ1bik7XG4gIH1cblxuICBzY2hlZHVsZShmdW4sIHBpZCl7XG4gICAgY29uc3QgdGhlX3BpZCA9IHBpZCAhPSBudWxsID8gcGlkIDogdGhpcy5jdXJyZW50X3Byb2Nlc3MucGlkO1xuICAgIHRoaXMuc2NoZWR1bGVyLnNjaGVkdWxlKHRoZV9waWQsIGZ1bik7XG4gIH1cblxuICBleGl0KG9uZSwgdHdvKXtcbiAgICBsZXQgcGlkID0gbnVsbDtcbiAgICBsZXQgcmVhc29uID0gbnVsbDtcbiAgICBsZXQgcHJvY2VzcyA9IG51bGw7XG5cbiAgICBpZih0d28pe1xuICAgICAgbGV0IHBpZCA9IG9uZTtcbiAgICAgIGxldCByZWFzb24gPSB0d287XG4gICAgICBsZXQgcHJvY2VzcyA9IHRoaXMucGlkcy5nZXQodGhpcy5waWRvZihwaWQpKTtcblxuICAgICAgaWYoKHByb2Nlc3MgJiYgcHJvY2Vzcy5pc190cmFwcGluZ19leGl0cygpKSB8fCByZWFzb24gPT09IFN0YXRlcy5LSUxMIHx8IHJlYXNvbiA9PT0gU3RhdGVzLk5PUk1BTCl7XG4gICAgICAgIHRoaXMubWFpbGJveGVzLmdldChwcm9jZXNzLnBpZCkuZGVsaXZlcihbU3RhdGVzLkVYSVQsIHRoaXMucGlkKCksIHJlYXNvbiBdKTtcbiAgICAgIH0gZWxzZXtcbiAgICAgICAgcHJvY2Vzcy5zaWduYWwocmVhc29uKTtcbiAgICAgIH1cblxuICAgIH1lbHNle1xuICAgICAgbGV0IHBpZCA9IHRoaXMuY3VycmVudF9wcm9jZXNzLnBpZDtcbiAgICAgIGxldCByZWFzb24gPSBvbmU7XG4gICAgICBsZXQgcHJvY2VzcyA9IHRoaXMuY3VycmVudF9wcm9jZXM7XG4gICAgICBwcm9jZXNzLnNpZ25hbChyZWFzb24pO1xuICAgIH1cblxuICAgIGZvcihsZXQgcmVmIGluIHByb2Nlc3MubW9uaXRvcnMpe1xuICAgICAgbGV0IG1vbnMgPSB0aGlzLm1vbml0b3JzLmdldChyZWYpO1xuICAgICAgdGhpcy5zZW5kKG1vbnNbJ21vbml0b3InXSwgWydET1dOJywgcmVmLCBtb25zWydtb25pdGVlJ10sIG1vbnNbJ21vbml0ZWUnXSwgcmVhc29uXSk7XG4gICAgfVxuICB9XG5cbiAgZXJyb3IocmVhc29uKXtcbiAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5zaWduYWwocmVhc29uKTtcbiAgfVxuXG4gIHByb2Nlc3NfZmxhZyguLi5hcmdzKXtcbiAgICBpZihhcmdzLmxlbmd0aCA9PSAyKXtcbiAgICAgIGNvbnN0IGZsYWcgPSBhcmdzWzBdO1xuICAgICAgY29uc3QgdmFsdWUgPSBhcmdzWzFdO1xuICAgICAgcmV0dXJuIHRoaXMuY3VycmVudF9wcm9jZXNzLnByb2Nlc3NfZmxhZyhmbGFnLCB2YWx1ZSk7XG4gICAgfWVsc2V7XG4gICAgICBjb25zdCBwaWQgPSB0aGlzLnBpZG9mKGFyZ3NbMF0pO1xuICAgICAgY29uc3QgZmxhZyA9IGFyZ3NbMV07XG4gICAgICBjb25zdCB2YWx1ZSA9IGFyZ3NbMl07XG4gICAgICByZXR1cm4gdGhpcy5waWRzLmdldChwaWQpLnByb2Nlc3NfZmxhZyhmbGFnLCB2YWx1ZSk7XG4gICAgfVxuICB9XG5cbiAgcHV0KGtleSwgdmFsdWUpe1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3Rba2V5XSA9IHZhbHVlO1xuICB9XG5cbiAgZ2V0X3Byb2Nlc3NfZGljdCgpe1xuICAgIHJldHVybiB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0O1xuICB9XG5cbiAgZ2V0KGtleSwgZGVmYXVsdF92YWx1ZSA9IG51bGwpe1xuICAgIGlmKGtleSBpbiB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0KXtcbiAgICAgIHJldHVybiB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0W2tleV07XG4gICAgfWVsc2V7XG4gICAgICByZXR1cm4gZGVmYXVsdF92YWx1ZTtcbiAgICB9XG4gIH1cblxuICBnZXRfa2V5cyh2YWx1ZSl7XG4gICAgaWYodmFsdWUpe1xuICAgICAgbGV0IGtleXMgPSBbXTtcblxuICAgICAgZm9yKGxldCBrZXkgb2YgT2JqZWN0LmtleXModGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdCkpe1xuICAgICAgICBpZih0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0W2tleV0gPT09IHZhbHVlKXtcbiAgICAgICAgICBrZXlzLnB1c2goa2V5KTtcbiAgICAgICAgfVxuICAgICAgfVxuXG4gICAgICByZXR1cm4ga2V5cztcbiAgICB9XG5cbiAgICByZXR1cm4gT2JqZWN0LmtleXModGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdCk7XG4gIH1cblxuICBlcmFzZShrZXkpe1xuICAgIGlmKGtleSAhPSBudWxsKXtcbiAgICAgIGRlbGV0ZSB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0W2tleV07XG4gICAgfWVsc2V7XG4gICAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0ID0ge307XG4gICAgfVxuICB9XG5cbiAgaXNfYWxpdmUocGlkKXtcbiAgICBjb25zdCByZWFsX3BpZCA9IHRoaXMucGlkb2YocGlkKTtcbiAgICByZXR1cm4gcmVhbF9waWQgIT0gbnVsbDtcbiAgfVxuXG4gIGxpc3QoKXtcbiAgICByZXR1cm4gdGhpcy5waWRzLmtleXMoKTtcbiAgfVxuXG4gIG1ha2VfcmVmKCl7XG4gICAgcmV0dXJuIG5ldyBFcmxhbmdUeXBlcy5SZWZlcmVuY2UoKTtcbiAgfVxufVxuXG5leHBvcnQgZGVmYXVsdCBQcm9jZXNzU3lzdGVtO1xuIiwiaW1wb3J0IFByb2Nlc3NTeXN0ZW0gZnJvbSBcIi4vcHJvY2Vzc2VzL3Byb2Nlc3Nfc3lzdGVtXCI7XG5cbmV4cG9ydCBkZWZhdWx0IHtcbiAgUHJvY2Vzc1N5c3RlbVxufTtcbiJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiOzs7Ozs7OztBQUlBLE1BQU0sT0FBTixDQUFhO2dCQUNFO1NBQ04sUUFBTCxHQUFnQixFQUFoQjs7O1VBR00sT0FBUixFQUFnQjtTQUNULFFBQUwsQ0FBYyxJQUFkLENBQW1CLE9BQW5CO1dBQ08sT0FBUDs7O1FBR0c7V0FDSSxLQUFLLFFBQVo7OztZQUdPO1dBQ0EsS0FBSyxRQUFMLENBQWMsTUFBZCxLQUF5QixDQUFoQzs7O1dBR08sS0FBVCxFQUFlO1NBQ1IsUUFBTCxDQUFjLE1BQWQsQ0FBcUIsS0FB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