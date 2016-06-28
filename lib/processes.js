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
      yield process_system_scope.sleep(Symbol.for("Infinity"));
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
      this.pids.get(real_pid).monitors.push(ref);
      return ref;
    } else {
      this.send(this.current_process.pid, new ErlangTypes.Tuple('DOWN', ref, pid, real_pid, Symbol.for('noproc')));
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

    if (Number.isInteger(time)) {
      this.scheduler.scheduleFuture(this.current_process.pid, time, fun);
    }
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
      pid = one;
      reason = two;
      process = this.pids.get(this.pidof(pid));

      if (process && process.is_trapping_exits() || reason === States.KILL || reason === States.NORMAL) {
        this.mailboxes.get(process.pid).deliver(new ErlangTypes.Tuple(States.EXIT, this.pid(), reason));
      } else {
        process.signal(reason);
      }
    } else {
      pid = this.current_process.pid;
      reason = one;
      process = this.current_process;

      process.signal(reason);
    }

    for (let ref in process.monitors) {
      let mons = this.monitors.get(ref);
      this.send(mons['monitor'], new ErlangTypes.Tuple('DOWN', ref, mons['monitee'], mons['monitee'], reason));
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
    return Array.from(this.pids.keys());
  }

  make_ref() {
    return new ErlangTypes.Reference();
  }
}

var index = {
  ProcessSystem
};

module.exports = index;
//# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjpudWxsLCJzb3VyY2VzIjpbIi4uL3NyYy9wcm9jZXNzZXMvbWFpbGJveC5qcyIsIi4uL3NyYy9wcm9jZXNzZXMvc3RhdGVzLmpzIiwiLi4vc3JjL3Byb2Nlc3Nlcy9wcm9jZXNzLmpzIiwiLi4vc3JjL3Byb2Nlc3Nlcy9zY2hlZHVsZXIuanMiLCIuLi9zcmMvcHJvY2Vzc2VzL3Byb2Nlc3Nfc3lzdGVtLmpzIiwiLi4vc3JjL2luZGV4LmpzIl0sInNvdXJjZXNDb250ZW50IjpbIlwidXNlIHN0cmljdFwiO1xuXG4vKiBAZmxvdyAqL1xuXG5jbGFzcyBNYWlsYm94e1xuICBjb25zdHJ1Y3Rvcigpe1xuICAgIHRoaXMubWVzc2FnZXMgPSBbXTtcbiAgfVxuXG4gIGRlbGl2ZXIobWVzc2FnZSl7XG4gICAgdGhpcy5tZXNzYWdlcy5wdXNoKG1lc3NhZ2UpO1xuICAgIHJldHVybiBtZXNzYWdlO1xuICB9XG5cbiAgZ2V0KCl7XG4gICAgcmV0dXJuIHRoaXMubWVzc2FnZXM7XG4gIH1cblxuICBpc0VtcHR5KCl7XG4gICAgcmV0dXJuIHRoaXMubWVzc2FnZXMubGVuZ3RoID09PSAwO1xuICB9XG5cbiAgcmVtb3ZlQXQoaW5kZXgpe1xuICAgIHRoaXMubWVzc2FnZXMuc3BsaWNlKGluZGV4LCAxKTtcbiAgfVxufVxuXG5leHBvcnQgZGVmYXVsdCBNYWlsYm94O1xuIiwiZXhwb3J0IGRlZmF1bHQge1xuICBOT1JNQUw6IFN5bWJvbC5mb3IoXCJub3JtYWxcIiksXG4gIEtJTEw6IFN5bWJvbC5mb3IoXCJraWxsXCIpLFxuICBTVVNQRU5EOiBTeW1ib2wuZm9yKFwic3VzcGVuZFwiKSxcbiAgQ09OVElOVUU6IFN5bWJvbC5mb3IoXCJjb250aW51ZVwiKSxcbiAgUkVDRUlWRTogU3ltYm9sLmZvcihcInJlY2VpdmVcIiksXG4gIFNFTkQ6IFN5bWJvbC5mb3IoXCJzZW5kXCIpLFxuICBTTEVFUElORzogU3ltYm9sLmZvcihcInNsZWVwaW5nXCIpLFxuICBSVU5OSU5HOiBTeW1ib2wuZm9yKFwicnVubmluZ1wiKSxcbiAgU1VTUEVOREVEOiBTeW1ib2wuZm9yKFwic3VzcGVuZGVkXCIpLFxuICBTVE9QUEVEOiBTeW1ib2wuZm9yKFwic3RvcHBlZFwiKSxcbiAgU0xFRVA6IFN5bWJvbC5mb3IoXCJzbGVlcFwiKSxcbiAgRVhJVDogU3ltYm9sLmZvcihcImV4aXRcIiksXG4gIE5PTUFUQ0g6IFN5bWJvbC5mb3IoXCJub19tYXRjaFwiKVxufSIsIlwidXNlIHN0cmljdFwiO1xuXG4vKiBAZmxvdyAqL1xuaW1wb3J0IE1haWxib3ggZnJvbSBcIi4vbWFpbGJveFwiO1xuaW1wb3J0IFByb2Nlc3NTeXN0ZW0gZnJvbSBcIi4vcHJvY2Vzc19zeXN0ZW1cIjtcbmltcG9ydCBTdGF0ZXMgZnJvbSBcIi4vc3RhdGVzXCI7XG5cbmZ1bmN0aW9uIGlzX3NsZWVwKHZhbHVlKXtcbiAgcmV0dXJuIEFycmF5LmlzQXJyYXkodmFsdWUpICYmIHZhbHVlWzBdID09PSBTdGF0ZXMuU0xFRVA7XG59XG5cbmZ1bmN0aW9uIGlzX3JlY2VpdmUodmFsdWUpe1xuICByZXR1cm4gQXJyYXkuaXNBcnJheSh2YWx1ZSkgJiYgdmFsdWVbMF0gPT09IFN0YXRlcy5SRUNFSVZFO1xufVxuXG5mdW5jdGlvbiByZWNlaXZlX3RpbWVkX291dCh2YWx1ZSl7XG4gIHJldHVybiB2YWx1ZVsyXSAhPSBudWxsICYmIHZhbHVlWzJdIDwgRGF0ZS5ub3coKTtcbn1cblxuY2xhc3MgUHJvY2VzcyB7XG4gIGNvbnN0cnVjdG9yKHBpZCwgZnVuYywgYXJncywgbWFpbGJveCwgc3lzdGVtKXtcbiAgICB0aGlzLnBpZCA9IHBpZDtcbiAgICB0aGlzLmZ1bmMgPSBmdW5jO1xuICAgIHRoaXMuYXJncyA9IGFyZ3M7XG4gICAgdGhpcy5tYWlsYm94ID0gbWFpbGJveDtcbiAgICB0aGlzLnN5c3RlbSA9IHN5c3RlbTtcbiAgICB0aGlzLnN0YXR1cyA9IFN0YXRlcy5TVE9QUEVEO1xuICAgIHRoaXMuZGljdCA9IHt9O1xuICAgIHRoaXMuZmxhZ3MgPSB7fTtcbiAgICB0aGlzLm1vbml0b3JzID0gW107XG4gIH1cblxuICBzdGFydCgpe1xuICAgIGNvbnN0IGZ1bmN0aW9uX3Njb3BlID0gdGhpcztcbiAgICBsZXQgbWFjaGluZSA9IHRoaXMubWFpbigpO1xuXG4gICAgdGhpcy5zeXN0ZW0uc2NoZWR1bGUoZnVuY3Rpb24oKSB7XG4gICAgICBmdW5jdGlvbl9zY29wZS5zeXN0ZW0uc2V0X2N1cnJlbnQoZnVuY3Rpb25fc2NvcGUucGlkKTtcbiAgICAgIGZ1bmN0aW9uX3Njb3BlLnJ1bihtYWNoaW5lLCBtYWNoaW5lLm5leHQoKSk7XG4gICAgfSwgdGhpcy5waWQpO1xuICB9XG5cbiAgKm1haW4oKSB7XG4gICAgbGV0IHJldHZhbCA9IFN0YXRlcy5OT1JNQUw7XG5cbiAgICB0cnkge1xuICAgICAgeWllbGQqIHRoaXMuZnVuYy5hcHBseShudWxsLCB0aGlzLmFyZ3MpO1xuICAgIH0gY2F0Y2goZSkge1xuICAgICAgY29uc29sZS5lcnJvcihlKTtcbiAgICAgIHJldHZhbCA9IGU7XG4gICAgfVxuXG4gICAgdGhpcy5zeXN0ZW0uZXhpdChyZXR2YWwpO1xuICB9XG5cbiAgcHJvY2Vzc19mbGFnKGZsYWcsIHZhbHVlKXtcbiAgICBjb25zdCBvbGRfdmFsdWUgPSB0aGlzLmZsYWdzW2ZsYWddO1xuICAgIHRoaXMuZmxhZ3NbZmxhZ10gPSB2YWx1ZTtcbiAgICByZXR1cm4gb2xkX3ZhbHVlO1xuICB9XG5cbiAgaXNfdHJhcHBpbmdfZXhpdHMoKXtcbiAgICByZXR1cm4gdGhpcy5mbGFnc1tTeW1ib2wuZm9yKFwidHJhcF9leGl0XCIpXSAmJiB0aGlzLmZsYWdzW1N5bWJvbC5mb3IoXCJ0cmFwX2V4aXRcIildID09IHRydWU7XG4gIH1cblxuICBzaWduYWwocmVhc29uKXtcbiAgICBpZihyZWFzb24gIT09IFN0YXRlcy5OT1JNQUwpe1xuICAgICAgY29uc29sZS5lcnJvcihyZWFzb24pO1xuICAgIH1cblxuICAgIHRoaXMuc3lzdGVtLnJlbW92ZV9wcm9jKHRoaXMucGlkLCByZWFzb24pO1xuICB9XG5cbiAgcmVjZWl2ZShmdW4pe1xuICAgIGxldCB2YWx1ZSA9IFN0YXRlcy5OT01BVENIO1xuICAgIGxldCBtZXNzYWdlcyA9IHRoaXMubWFpbGJveC5nZXQoKTtcblxuICAgIGZvcihsZXQgaSA9IDA7IGkgPCBtZXNzYWdlcy5sZW5ndGg7IGkrKyl7XG4gICAgICB0cnl7XG4gICAgICAgIHZhbHVlID0gZnVuKG1lc3NhZ2VzW2ldKTtcbiAgICAgICAgaWYodmFsdWUgIT09IFN0YXRlcy5OT01BVENIKXtcbiAgICAgICAgICB0aGlzLm1haWxib3gucmVtb3ZlQXQoaSk7XG4gICAgICAgICAgYnJlYWs7XG4gICAgICAgIH1cbiAgICAgIH1jYXRjaChlKXtcbiAgICAgICAgaWYoZS5jb25zdHJ1Y3Rvci5uYW1lICE9IFwiTWF0Y2hFcnJvclwiKXtcbiAgICAgICAgICB0aGlzLmV4aXQoZSk7XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICB9XG5cbiAgICByZXR1cm4gdmFsdWU7XG4gIH1cblxuICBydW4obWFjaGluZSwgc3RlcCl7XG4gICAgY29uc3QgZnVuY3Rpb25fc2NvcGUgPSB0aGlzO1xuXG4gICAgaWYoIXN0ZXAuZG9uZSl7XG4gICAgICBsZXQgdmFsdWUgPSBzdGVwLnZhbHVlO1xuXG4gICAgICBpZihpc19zbGVlcCh2YWx1ZSkpe1xuXG4gICAgICAgIHRoaXMuc3lzdGVtLmRlbGF5KGZ1bmN0aW9uKCkge1xuICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnN5c3RlbS5zZXRfY3VycmVudChmdW5jdGlvbl9zY29wZS5waWQpO1xuICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnJ1bihtYWNoaW5lLCBtYWNoaW5lLm5leHQoKSk7XG4gICAgICAgIH0sIHZhbHVlWzFdKTtcblxuICAgICAgfWVsc2UgaWYoaXNfcmVjZWl2ZSh2YWx1ZSkgJiYgcmVjZWl2ZV90aW1lZF9vdXQodmFsdWUpKXtcblxuICAgICAgICBsZXQgcmVzdWx0ID0gdmFsdWVbM10oKTtcblxuICAgICAgICB0aGlzLnN5c3RlbS5zY2hlZHVsZShmdW5jdGlvbigpIHtcbiAgICAgICAgICBmdW5jdGlvbl9zY29wZS5zeXN0ZW0uc2V0X2N1cnJlbnQoZnVuY3Rpb25fc2NvcGUucGlkKTtcbiAgICAgICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgbWFjaGluZS5uZXh0KHJlc3VsdCkpO1xuICAgICAgICB9KTtcblxuICAgICAgfWVsc2UgaWYoaXNfcmVjZWl2ZSh2YWx1ZSkpe1xuXG4gICAgICAgIGxldCByZXN1bHQgPSBmdW5jdGlvbl9zY29wZS5yZWNlaXZlKHZhbHVlWzFdKTtcblxuICAgICAgICBpZihyZXN1bHQgPT09IFN0YXRlcy5OT01BVENIKXtcbiAgICAgICAgICB0aGlzLnN5c3RlbS5zdXNwZW5kKGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICAgICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgc3RlcCk7XG4gICAgICAgICAgfSk7XG4gICAgICAgIH1lbHNle1xuICAgICAgICAgIHRoaXMuc3lzdGVtLnNjaGVkdWxlKGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICAgICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgbWFjaGluZS5uZXh0KHJlc3VsdCkpO1xuICAgICAgICAgIH0pO1xuICAgICAgICB9XG5cbiAgICAgIH1lbHNle1xuICAgICAgICB0aGlzLnN5c3RlbS5zY2hlZHVsZShmdW5jdGlvbigpIHtcbiAgICAgICAgICBmdW5jdGlvbl9zY29wZS5zeXN0ZW0uc2V0X2N1cnJlbnQoZnVuY3Rpb25fc2NvcGUucGlkKTtcbiAgICAgICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgbWFjaGluZS5uZXh0KHZhbHVlKSk7XG4gICAgICAgIH0pO1xuICAgICAgfVxuICAgIH1cbiAgfVxufVxuXG5leHBvcnQgZGVmYXVsdCBQcm9jZXNzO1xuIiwiXCJ1c2Ugc3RyaWN0XCI7XG5cbmNsYXNzIFByb2Nlc3NRdWV1ZSB7XG4gIGNvbnN0cnVjdG9yKHBpZCl7XG4gICAgdGhpcy5waWQgPSBwaWQ7XG4gICAgdGhpcy50YXNrcyA9IFtdO1xuICB9XG5cbiAgZW1wdHkoKXtcbiAgICByZXR1cm4gdGhpcy50YXNrcy5sZW5ndGggPT09IDA7XG4gIH1cblxuICBhZGQodGFzayl7XG4gICAgdGhpcy50YXNrcy5wdXNoKHRhc2spO1xuICB9XG5cbiAgbmV4dCgpe1xuICAgIHJldHVybiB0aGlzLnRhc2tzLnNoaWZ0KCk7XG4gIH1cbn1cblxuY2xhc3MgU2NoZWR1bGVyIHtcbiAgICBjb25zdHJ1Y3Rvcih0aHJvdHRsZSA9IDAsIHJlZHVjdGlvbnNfcGVyX3Byb2Nlc3MgPSA4KXtcbiAgICAgICAgdGhpcy5pc1J1bm5pbmcgPSBmYWxzZTtcbiAgICAgICAgdGhpcy5pbnZva2VMYXRlciA9IGZ1bmN0aW9uIChjYWxsYmFjaykgeyBzZXRUaW1lb3V0KGNhbGxiYWNrLCB0aHJvdHRsZSk7IH07XG5cbiAgICAgICAgLy8gSW4gb3VyIGNhc2UgYSByZWR1Y3Rpb24gaXMgZXF1YWwgdG8gYSB0YXNrIGNhbGxcbiAgICAgICAgLy8gQ29udHJvbHMgaG93IG1hbnkgdGFza3MgYXJlIGNhbGxlZCBhdCBhIHRpbWUgcGVyIHByb2Nlc3NcbiAgICAgICAgdGhpcy5yZWR1Y3Rpb25zX3Blcl9wcm9jZXNzID0gcmVkdWN0aW9uc19wZXJfcHJvY2VzcztcbiAgICAgICAgdGhpcy5xdWV1ZXMgPSBuZXcgTWFwKCk7XG4gICAgICAgIHRoaXMucnVuKCk7XG4gIH1cblxuICBhZGRUb1F1ZXVlKHBpZCwgdGFzayl7XG4gICAgaWYoIXRoaXMucXVldWVzLmhhcyhwaWQpKXtcbiAgICAgIHRoaXMucXVldWVzLnNldChwaWQsIG5ldyBQcm9jZXNzUXVldWUocGlkKSk7XG4gICAgfVxuXG4gICAgdGhpcy5xdWV1ZXMuZ2V0KHBpZCkuYWRkKHRhc2spO1xuICB9XG5cbiAgcmVtb3ZlUGlkKHBpZCl7XG4gICAgdGhpcy5pc1J1bm5pbmcgPSB0cnVlO1xuXG4gICAgdGhpcy5xdWV1ZXMuZGVsZXRlKHBpZCk7XG5cbiAgICB0aGlzLmlzUnVubmluZyA9IGZhbHNlO1xuICB9XG5cbiAgcnVuKCl7XG4gICAgaWYgKHRoaXMuaXNSdW5uaW5nKSB7XG4gICAgICB0aGlzLmludm9rZUxhdGVyKCgpID0+IHsgdGhpcy5ydW4oKTsgfSk7XG4gICAgfSBlbHNlIHtcbiAgICAgIGZvcihsZXQgW3BpZCwgcXVldWVdIG9mIHRoaXMucXVldWVzKXtcbiAgICAgICAgbGV0IHJlZHVjdGlvbnMgPSAwO1xuICAgICAgICB3aGlsZShxdWV1ZSAmJiAhcXVldWUuZW1wdHkoKSAmJiByZWR1Y3Rpb25zIDwgdGhpcy5yZWR1Y3Rpb25zX3Blcl9wcm9jZXNzKXtcbiAgICAgICAgICBsZXQgdGFzayA9IHF1ZXVlLm5leHQoKTtcbiAgICAgICAgICB0aGlzLmlzUnVubmluZyA9IHRydWU7XG5cbiAgICAgICAgICBsZXQgcmVzdWx0O1xuXG4gICAgICAgICAgdHJ5e1xuICAgICAgICAgICAgcmVzdWx0ID0gdGFzaygpO1xuICAgICAgICAgIH1jYXRjaChlKXtcbiAgICAgICAgICAgIGNvbnNvbGUuZXJyb3IoZSk7XG4gICAgICAgICAgICByZXN1bHQgPSBlO1xuICAgICAgICAgIH1cblxuICAgICAgICAgIHRoaXMuaXNSdW5uaW5nID0gZmFsc2U7XG5cbiAgICAgICAgICBpZiAocmVzdWx0IGluc3RhbmNlb2YgRXJyb3IpIHtcbiAgICAgICAgICAgIHRocm93IHJlc3VsdDtcbiAgICAgICAgICB9XG5cbiAgICAgICAgICByZWR1Y3Rpb25zKys7XG4gICAgICAgIH1cbiAgICAgIH1cblxuICAgICAgdGhpcy5pbnZva2VMYXRlcigoKSA9PiB7IHRoaXMucnVuKCk7IH0pO1xuICAgIH1cbiAgfVxuXG4gIGFkZFRvU2NoZWR1bGVyKHBpZCwgdGFzaywgZHVlVGltZSA9IDApIHtcbiAgICBpZihkdWVUaW1lID09PSAwKXtcbiAgICAgIHRoaXMuaW52b2tlTGF0ZXIoKCkgPT4ge1xuICAgICAgICB0aGlzLmFkZFRvUXVldWUocGlkLCB0YXNrKTtcbiAgICAgIH0pO1xuICAgIH1lbHNle1xuICAgICAgc2V0VGltZW91dCgoKSA9PiB7XG4gICAgICAgIHRoaXMuYWRkVG9RdWV1ZShwaWQsIHRhc2spO1xuICAgICAgfSwgZHVlVGltZSk7XG4gICAgfVxuICB9O1xuXG4gIHNjaGVkdWxlKHBpZCwgdGFzayl7XG4gICAgdGhpcy5hZGRUb1NjaGVkdWxlcihwaWQsICgpID0+IHsgdGFzaygpOyB9KTtcbiAgfVxuXG4gIHNjaGVkdWxlRnV0dXJlKHBpZCwgZHVlVGltZSwgdGFzayl7XG4gICAgdGhpcy5hZGRUb1NjaGVkdWxlcihwaWQsICgpID0+IHsgdGFzaygpOyB9LCBkdWVUaW1lKTtcbiAgfVxufVxuXG5leHBvcnQgZGVmYXVsdCBTY2hlZHVsZXI7XG4iLCIvKiBAZmxvdyAqL1xuXCJ1c2Ugc3RyaWN0XCI7XG5cbmltcG9ydCBNYWlsYm94IGZyb20gXCIuL21haWxib3hcIjtcbmltcG9ydCBQcm9jZXNzIGZyb20gXCIuL3Byb2Nlc3NcIjtcbmltcG9ydCBTdGF0ZXMgZnJvbSBcIi4vc3RhdGVzXCI7XG5pbXBvcnQgU2NoZWR1bGVyIGZyb20gXCIuL3NjaGVkdWxlclwiO1xuaW1wb3J0IEVybGFuZ1R5cGVzIGZyb20gXCJlcmxhbmctdHlwZXNcIjtcblxuXG5jbGFzcyBQcm9jZXNzU3lzdGVtIHtcblxuICBjb25zdHJ1Y3Rvcigpe1xuICAgIHRoaXMucGlkcyA9IG5ldyBNYXAoKTtcbiAgICB0aGlzLm1haWxib3hlcyA9IG5ldyBNYXAoKTtcbiAgICB0aGlzLm5hbWVzID0gbmV3IE1hcCgpO1xuICAgIHRoaXMubGlua3MgPSBuZXcgTWFwKCk7XG4gICAgdGhpcy5tb25pdG9ycyA9IG5ldyBNYXAoKTtcblxuICAgIGNvbnN0IHRocm90dGxlID0gNTsgLy9tcyBiZXR3ZWVuIHNjaGVkdWxlZCB0YXNrc1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzID0gbnVsbDtcbiAgICB0aGlzLnNjaGVkdWxlciA9IG5ldyBTY2hlZHVsZXIodGhyb3R0bGUpO1xuICAgIHRoaXMuc3VzcGVuZGVkID0gbmV3IE1hcCgpO1xuXG4gICAgbGV0IHByb2Nlc3Nfc3lzdGVtX3Njb3BlID0gdGhpcztcbiAgICB0aGlzLm1haW5fcHJvY2Vzc19waWQgPSB0aGlzLnNwYXduKGZ1bmN0aW9uKigpe1xuICAgICAgeWllbGQgcHJvY2Vzc19zeXN0ZW1fc2NvcGUuc2xlZXAoU3ltYm9sLmZvcihcIkluZmluaXR5XCIpKTtcbiAgICB9KTtcbiAgICB0aGlzLnNldF9jdXJyZW50KHRoaXMubWFpbl9wcm9jZXNzX3BpZCk7XG4gIH1cblxuICBzdGF0aWMgKiBydW4oZnVuLCBhcmdzLCBjb250ZXh0ID0gbnVsbCl7XG4gICAgaWYoZnVuLmNvbnN0cnVjdG9yLm5hbWUgPT09IFwiR2VuZXJhdG9yRnVuY3Rpb25cIil7XG4gICAgICByZXR1cm4geWllbGQqIGZ1bi5hcHBseShjb250ZXh0LCBhcmdzKTtcbiAgICB9ZWxzZXtcbiAgICAgIHJldHVybiB5aWVsZCBmdW4uYXBwbHkoY29udGV4dCwgYXJncyk7XG4gICAgfVxuICB9XG5cbiAgc3Bhd24oLi4uYXJncyl7XG4gICAgaWYoYXJncy5sZW5ndGggPT09IDEpe1xuICAgICAgbGV0IGZ1biA9IGFyZ3NbMF07XG4gICAgICByZXR1cm4gdGhpcy5hZGRfcHJvYyhmdW4sIFtdLCBmYWxzZSkucGlkO1xuXG4gICAgfWVsc2V7XG4gICAgICBsZXQgbW9kID0gYXJnc1swXTtcbiAgICAgIGxldCBmdW4gPSBhcmdzWzFdO1xuICAgICAgbGV0IHRoZV9hcmdzID0gYXJnc1syXTtcblxuICAgICAgcmV0dXJuIHRoaXMuYWRkX3Byb2MobW9kW2Z1bl0sIHRoZV9hcmdzLCBmYWxzZSwgZmFsc2UpLnBpZDtcbiAgICB9XG4gIH1cblxuICBzcGF3bl9saW5rKC4uLmFyZ3Mpe1xuICAgIGlmKGFyZ3MubGVuZ3RoID09PSAxKXtcbiAgICAgIGxldCBmdW4gPSBhcmdzWzBdO1xuICAgICAgcmV0dXJuIHRoaXMuYWRkX3Byb2MoZnVuLCBbXSwgdHJ1ZSwgZmFsc2UpLnBpZDtcblxuICAgIH1lbHNle1xuICAgICAgbGV0IG1vZCA9IGFyZ3NbMF07XG4gICAgICBsZXQgZnVuID0gYXJnc1sxXTtcbiAgICAgIGxldCB0aGVfYXJncyA9IGFyZ3NbMl07XG5cbiAgICAgIHJldHVybiB0aGlzLmFkZF9wcm9jKG1vZFtmdW5dLCB0aGVfYXJncywgdHJ1ZSwgZmFsc2UpLnBpZDtcbiAgICB9XG4gIH1cblxuICBsaW5rKHBpZCl7XG4gICAgdGhpcy5saW5rcy5nZXQodGhpcy5waWQoKSkuYWRkKHBpZCk7XG4gICAgdGhpcy5saW5rcy5nZXQocGlkKS5hZGQodGhpcy5waWQoKSk7XG4gIH1cblxuICB1bmxpbmsocGlkKXtcbiAgICB0aGlzLmxpbmtzLmdldCh0aGlzLnBpZCgpKS5kZWxldGUocGlkKTtcbiAgICB0aGlzLmxpbmtzLmdldChwaWQpLmRlbGV0ZSh0aGlzLnBpZCgpKTtcbiAgfVxuXG4gIHNwYXduX21vbml0b3IoLi4uYXJncyl7XG4gICAgaWYoYXJncy5sZW5ndGggPT09IDEpe1xuICAgICAgbGV0IGZ1biA9IGFyZ3NbMF07XG4gICAgICBsZXQgcHJvY2VzcyA9IHRoaXMuYWRkX3Byb2MoZnVuLCBbXSwgZmFsc2UsIHRydWUpO1xuICAgICAgcmV0dXJuIFtwcm9jZXNzLnBpZCwgcHJvY2Vzcy5tb25pdG9yc1swXV07XG5cbiAgICB9ZWxzZXtcbiAgICAgIGxldCBtb2QgPSBhcmdzWzBdO1xuICAgICAgbGV0IGZ1biA9IGFyZ3NbMV07XG4gICAgICBsZXQgdGhlX2FyZ3MgPSBhcmdzWzJdO1xuICAgICAgbGV0IHByb2Nlc3MgPSB0aGlzLmFkZF9wcm9jKG1vZFtmdW5dLCB0aGVfYXJncywgZmFsc2UsIHRydWUpO1xuXG4gICAgICByZXR1cm4gW3Byb2Nlc3MucGlkLCBwcm9jZXNzLm1vbml0b3JzWzBdXTtcbiAgICB9XG4gIH1cblxuICBtb25pdG9yKHBpZCl7XG4gICAgY29uc3QgcmVhbF9waWQgPSB0aGlzLnBpZG9mKHBpZCk7XG4gICAgY29uc3QgcmVmID0gdGhpcy5tYWtlX3JlZigpO1xuXG4gICAgaWYocmVhbF9waWQpe1xuXG4gICAgICB0aGlzLm1vbml0b3JzLnNldChyZWYsIHsnbW9uaXRvcic6IHRoaXMuY3VycmVudF9wcm9jZXNzLnBpZCwgJ21vbml0ZWUnOiByZWFsX3BpZH0pO1xuICAgICAgdGhpcy5waWRzLmdldChyZWFsX3BpZCkubW9uaXRvcnMucHVzaChyZWYpO1xuICAgICAgcmV0dXJuIHJlZjtcbiAgICB9ZWxzZXtcbiAgICAgIHRoaXMuc2VuZCh0aGlzLmN1cnJlbnRfcHJvY2Vzcy5waWQsIG5ldyBFcmxhbmdUeXBlcy5UdXBsZSgnRE9XTicsIHJlZiwgcGlkLCByZWFsX3BpZCwgU3ltYm9sLmZvcignbm9wcm9jJykpKTtcbiAgICAgIHJldHVybiByZWY7XG4gICAgfVxuICB9XG5cbiAgZGVtb25pdG9yKHJlZil7XG4gICAgaWYodGhpcy5tb25pdG9yLmhhcyhyZWYpKXtcbiAgICAgIHRoaXMubW9uaXRvci5kZWxldGUocmVmKTtcbiAgICAgIHJldHVybiB0cnVlO1xuICAgIH1cblxuICAgIHJldHVybiBmYWxzZTtcbiAgfVxuXG4gIHNldF9jdXJyZW50KGlkKXtcbiAgICBsZXQgcGlkID0gdGhpcy5waWRvZihpZCk7XG4gICAgaWYocGlkICE9PSBudWxsKXtcbiAgICAgIHRoaXMuY3VycmVudF9wcm9jZXNzID0gdGhpcy5waWRzLmdldChwaWQpO1xuICAgICAgdGhpcy5jdXJyZW50X3Byb2Nlc3Muc3RhdHVzID0gU3RhdGVzLlJVTk5JTkc7XG4gICAgfVxuICB9XG5cbiAgYWRkX3Byb2MoZnVuLCBhcmdzLCBsaW5rZWQsIG1vbml0b3JlZCl7XG4gICAgbGV0IG5ld3BpZCA9IG5ldyBFcmxhbmdUeXBlcy5QSUQoKTtcbiAgICBsZXQgbWFpbGJveCA9IG5ldyBNYWlsYm94KCk7XG4gICAgbGV0IG5ld3Byb2MgPSBuZXcgUHJvY2VzcyhuZXdwaWQsIGZ1biwgYXJncywgbWFpbGJveCwgdGhpcyk7XG5cbiAgICB0aGlzLnBpZHMuc2V0KG5ld3BpZCwgbmV3cHJvYyk7XG4gICAgdGhpcy5tYWlsYm94ZXMuc2V0KG5ld3BpZCwgbWFpbGJveCk7XG4gICAgdGhpcy5saW5rcy5zZXQobmV3cGlkLCBuZXcgU2V0KCkpO1xuXG4gICAgaWYobGlua2VkKXtcbiAgICAgIHRoaXMubGluayhuZXdwaWQpO1xuICAgIH1cblxuICAgIGlmKG1vbml0b3JlZCl7XG4gICAgICB0aGlzLm1vbml0b3IobmV3cGlkKTtcbiAgICB9XG5cbiAgICBuZXdwcm9jLnN0YXJ0KCk7XG4gICAgcmV0dXJuIG5ld3Byb2M7XG4gIH1cblxuICByZW1vdmVfcHJvYyhwaWQsIGV4aXRyZWFzb24pe1xuICAgIHRoaXMucGlkcy5kZWxldGUocGlkKTtcbiAgICB0aGlzLnVucmVnaXN0ZXIocGlkKTtcbiAgICB0aGlzLnNjaGVkdWxlci5yZW1vdmVQaWQocGlkKTtcblxuICAgIGlmKHRoaXMubGlua3MuaGFzKHBpZCkpe1xuICAgICAgZm9yIChsZXQgbGlua3BpZCBvZiB0aGlzLmxpbmtzLmdldChwaWQpKSB7XG4gICAgICAgIHRoaXMuZXhpdChsaW5rcGlkLCBleGl0cmVhc29uKTtcbiAgICAgICAgdGhpcy5saW5rcy5nZXQobGlua3BpZCkuZGVsZXRlKHBpZCk7XG4gICAgICB9XG5cbiAgICAgIHRoaXMubGlua3MuZGVsZXRlKHBpZCk7XG4gICAgfVxuICB9XG5cbiAgcmVnaXN0ZXIobmFtZSwgcGlkKXtcbiAgICBpZighdGhpcy5uYW1lcy5oYXMobmFtZSkpe1xuICAgICAgdGhpcy5uYW1lcy5zZXQobmFtZSwgcGlkKTtcbiAgICB9ZWxzZXtcbiAgICAgIHRocm93IG5ldyBFcnJvcihcIk5hbWUgaXMgYWxyZWFkeSByZWdpc3RlcmVkIHRvIGFub3RoZXIgcHJvY2Vzc1wiKTtcbiAgICB9XG4gIH1cblxuICB3aGVyZWlzKG5hbWUpe1xuICAgIHJldHVybiB0aGlzLm5hbWVzLmhhcyhuYW1lKSA/IHRoaXMubmFtZXMuZ2V0KG5hbWUpIDogbnVsbDtcbiAgfVxuXG4gIHJlZ2lzdGVyZWQoKXtcbiAgICByZXR1cm4gdGhpcy5uYW1lcy5rZXlzKCk7XG4gIH1cblxuICB1bnJlZ2lzdGVyKHBpZCl7XG4gICAgZm9yKGxldCBuYW1lIG9mIHRoaXMubmFtZXMua2V5cygpKXtcbiAgICAgIGlmKHRoaXMubmFtZXMuaGFzKG5hbWUpICYmIHRoaXMubmFtZXMuZ2V0KG5hbWUpID09PSBwaWQpe1xuICAgICAgICB0aGlzLm5hbWVzLmRlbGV0ZShuYW1lKTtcbiAgICAgIH1cbiAgICB9XG4gIH1cblxuICBwaWQoKXtcbiAgICByZXR1cm4gdGhpcy5jdXJyZW50X3Byb2Nlc3MucGlkO1xuICB9XG5cbiAgcGlkb2YoaWQpe1xuICAgIGlmIChpZCBpbnN0YW5jZW9mIEVybGFuZ1R5cGVzLlBJRCkge1xuICAgICAgIHJldHVybiB0aGlzLnBpZHMuaGFzKGlkKSA/IGlkIDogbnVsbDtcbiAgICB9IGVsc2UgaWYgKGlkIGluc3RhbmNlb2YgUHJvY2Vzcykge1xuICAgICAgIHJldHVybiBpZC5waWQ7XG4gICAgfSBlbHNlIHtcbiAgICAgICBsZXQgcGlkID0gdGhpcy53aGVyZWlzKGlkKTtcbiAgICAgICBpZiAocGlkID09PSBudWxsKVxuICAgICAgICAgIHRocm93KFwiUHJvY2VzcyBuYW1lIG5vdCByZWdpc3RlcmVkOiBcIiArIGlkICsgXCIgKFwiICsgdHlwZW9mKGlkKSArIFwiKVwiKTtcbiAgICAgICByZXR1cm4gcGlkO1xuICAgIH1cbiAgfVxuXG4gIHNlbmQoaWQsIG1zZykge1xuICAgIGNvbnN0IHBpZCA9IHRoaXMucGlkb2YoaWQpO1xuXG4gICAgaWYocGlkKXtcbiAgICAgIHRoaXMubWFpbGJveGVzLmdldChwaWQpLmRlbGl2ZXIobXNnKTtcblxuICAgICAgaWYodGhpcy5zdXNwZW5kZWQuaGFzKHBpZCkpe1xuICAgICAgICBsZXQgZnVuID0gdGhpcy5zdXNwZW5kZWQuZ2V0KHBpZCk7XG4gICAgICAgIHRoaXMuc3VzcGVuZGVkLmRlbGV0ZShwaWQpO1xuICAgICAgICB0aGlzLnNjaGVkdWxlKGZ1bik7XG4gICAgICB9XG4gICAgfVxuXG4gICAgcmV0dXJuIG1zZztcbiAgfVxuXG4gIHJlY2VpdmUoZnVuLCB0aW1lb3V0ID0gMCwgdGltZW91dEZuID0gKCkgPT4gdHJ1ZSApIHtcbiAgICBsZXQgRGF0ZVRpbWVvdXQgPSBudWxsO1xuXG4gICAgaWYodGltZW91dCA9PT0gMCB8fCB0aW1lb3V0ID09PSBJbmZpbml0eSl7XG4gICAgICBEYXRlVGltZW91dCA9IG51bGw7XG4gICAgfWVsc2V7XG4gICAgICBEYXRlVGltZW91dCA9IERhdGUubm93KCkgKyB0aW1lb3V0O1xuICAgIH1cblxuICAgIHJldHVybiBbXG4gICAgICBTdGF0ZXMuUkVDRUlWRSxcbiAgICAgIGZ1bixcbiAgICAgIERhdGVUaW1lb3V0LFxuICAgICAgdGltZW91dEZuXG4gICAgXTtcbiAgfVxuXG4gIHNsZWVwKGR1cmF0aW9uKXtcbiAgICByZXR1cm4gW1N0YXRlcy5TTEVFUCwgZHVyYXRpb25dO1xuICB9XG5cbiAgc3VzcGVuZChmdW4pe1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLnN0YXR1cyA9IFN0YXRlcy5TVVNQRU5ERUQ7XG4gICAgdGhpcy5zdXNwZW5kZWQuc2V0KHRoaXMuY3VycmVudF9wcm9jZXNzLnBpZCwgZnVuKTtcbiAgfVxuXG4gIGRlbGF5KGZ1biwgdGltZSl7XG4gICAgdGhpcy5jdXJyZW50X3Byb2Nlc3Muc3RhdHVzID0gU3RhdGVzLlNMRUVQSU5HO1xuXG4gICAgaWYoTnVtYmVyLmlzSW50ZWdlcih0aW1lKSl7XG4gICAgICB0aGlzLnNjaGVkdWxlci5zY2hlZHVsZUZ1dHVyZSh0aGlzLmN1cnJlbnRfcHJvY2Vzcy5waWQsIHRpbWUsIGZ1bik7XG4gICAgfVxuICB9XG5cbiAgc2NoZWR1bGUoZnVuLCBwaWQpe1xuICAgIGNvbnN0IHRoZV9waWQgPSBwaWQgIT0gbnVsbCA/IHBpZCA6IHRoaXMuY3VycmVudF9wcm9jZXNzLnBpZDtcbiAgICB0aGlzLnNjaGVkdWxlci5zY2hlZHVsZSh0aGVfcGlkLCBmdW4pO1xuICB9XG5cbiAgZXhpdChvbmUsIHR3byl7XG4gICAgbGV0IHBpZCA9IG51bGw7XG4gICAgbGV0IHJlYXNvbiA9IG51bGw7XG4gICAgbGV0IHByb2Nlc3MgPSBudWxsO1xuXG4gICAgaWYodHdvKXtcbiAgICAgIHBpZCA9IG9uZTtcbiAgICAgIHJlYXNvbiA9IHR3bztcbiAgICAgIHByb2Nlc3MgPSB0aGlzLnBpZHMuZ2V0KHRoaXMucGlkb2YocGlkKSk7XG5cbiAgICAgIGlmKChwcm9jZXNzICYmIHByb2Nlc3MuaXNfdHJhcHBpbmdfZXhpdHMoKSkgfHwgcmVhc29uID09PSBTdGF0ZXMuS0lMTCB8fCByZWFzb24gPT09IFN0YXRlcy5OT1JNQUwpe1xuICAgICAgICB0aGlzLm1haWxib3hlcy5nZXQocHJvY2Vzcy5waWQpLmRlbGl2ZXIobmV3IEVybGFuZ1R5cGVzLlR1cGxlKFN0YXRlcy5FWElULCB0aGlzLnBpZCgpLCByZWFzb24gKSk7XG4gICAgICB9IGVsc2V7XG4gICAgICAgIHByb2Nlc3Muc2lnbmFsKHJlYXNvbik7XG4gICAgICB9XG5cbiAgICB9ZWxzZXtcbiAgICAgIHBpZCA9IHRoaXMuY3VycmVudF9wcm9jZXNzLnBpZDtcbiAgICAgIHJlYXNvbiA9IG9uZTtcbiAgICAgIHByb2Nlc3MgPSB0aGlzLmN1cnJlbnRfcHJvY2VzcztcblxuICAgICAgcHJvY2Vzcy5zaWduYWwocmVhc29uKTtcbiAgICB9XG5cbiAgICBmb3IobGV0IHJlZiBpbiBwcm9jZXNzLm1vbml0b3JzKXtcbiAgICAgIGxldCBtb25zID0gdGhpcy5tb25pdG9ycy5nZXQocmVmKTtcbiAgICAgIHRoaXMuc2VuZChtb25zWydtb25pdG9yJ10sIG5ldyBFcmxhbmdUeXBlcy5UdXBsZSgnRE9XTicsIHJlZiwgbW9uc1snbW9uaXRlZSddLCBtb25zWydtb25pdGVlJ10sIHJlYXNvbikpO1xuICAgIH1cbiAgfVxuXG4gIGVycm9yKHJlYXNvbil7XG4gICAgdGhpcy5jdXJyZW50X3Byb2Nlc3Muc2lnbmFsKHJlYXNvbik7XG4gIH1cblxuICBwcm9jZXNzX2ZsYWcoLi4uYXJncyl7XG4gICAgaWYoYXJncy5sZW5ndGggPT0gMil7XG4gICAgICBjb25zdCBmbGFnID0gYXJnc1swXTtcbiAgICAgIGNvbnN0IHZhbHVlID0gYXJnc1sxXTtcbiAgICAgIHJldHVybiB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5wcm9jZXNzX2ZsYWcoZmxhZywgdmFsdWUpO1xuICAgIH1lbHNle1xuICAgICAgY29uc3QgcGlkID0gdGhpcy5waWRvZihhcmdzWzBdKTtcbiAgICAgIGNvbnN0IGZsYWcgPSBhcmdzWzFdO1xuICAgICAgY29uc3QgdmFsdWUgPSBhcmdzWzJdO1xuICAgICAgcmV0dXJuIHRoaXMucGlkcy5nZXQocGlkKS5wcm9jZXNzX2ZsYWcoZmxhZywgdmFsdWUpO1xuICAgIH1cbiAgfVxuXG4gIHB1dChrZXksIHZhbHVlKXtcbiAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0W2tleV0gPSB2YWx1ZTtcbiAgfVxuXG4gIGdldF9wcm9jZXNzX2RpY3QoKXtcbiAgICByZXR1cm4gdGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdDtcbiAgfVxuXG4gIGdldChrZXksIGRlZmF1bHRfdmFsdWUgPSBudWxsKXtcbiAgICBpZihrZXkgaW4gdGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdCl7XG4gICAgICByZXR1cm4gdGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdFtrZXldO1xuICAgIH1lbHNle1xuICAgICAgcmV0dXJuIGRlZmF1bHRfdmFsdWU7XG4gICAgfVxuICB9XG5cbiAgZ2V0X2tleXModmFsdWUpe1xuICAgIGlmKHZhbHVlKXtcbiAgICAgIGxldCBrZXlzID0gW107XG5cbiAgICAgIGZvcihsZXQga2V5IG9mIE9iamVjdC5rZXlzKHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3QpKXtcbiAgICAgICAgaWYodGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdFtrZXldID09PSB2YWx1ZSl7XG4gICAgICAgICAga2V5cy5wdXNoKGtleSk7XG4gICAgICAgIH1cbiAgICAgIH1cblxuICAgICAgcmV0dXJuIGtleXM7XG4gICAgfVxuXG4gICAgcmV0dXJuIE9iamVjdC5rZXlzKHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3QpO1xuICB9XG5cbiAgZXJhc2Uoa2V5KXtcbiAgICBpZihrZXkgIT0gbnVsbCl7XG4gICAgICBkZWxldGUgdGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdFtrZXldO1xuICAgIH1lbHNle1xuICAgICAgdGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdCA9IHt9O1xuICAgIH1cbiAgfVxuXG4gIGlzX2FsaXZlKHBpZCl7XG4gICAgY29uc3QgcmVhbF9waWQgPSB0aGlzLnBpZG9mKHBpZCk7XG4gICAgcmV0dXJuIHJlYWxfcGlkICE9IG51bGw7XG4gIH1cblxuICBsaXN0KCl7XG4gICAgcmV0dXJuIEFycmF5LmZyb20odGhpcy5waWRzLmtleXMoKSk7XG4gIH1cblxuICBtYWtlX3JlZigpe1xuICAgIHJldHVybiBuZXcgRXJsYW5nVHlwZXMuUmVmZXJlbmNlKCk7XG4gIH1cbn1cblxuZXhwb3J0IGRlZmF1bHQgUHJvY2Vzc1N5c3RlbTtcbiIsImltcG9ydCBQcm9jZXNzU3lzdGVtIGZyb20gXCIuL3Byb2Nlc3Nlcy9wcm9jZXNzX3N5c3RlbVwiO1xuXG5leHBvcnQgZGVmYXVsdCB7XG4gIFByb2Nlc3NTeXN0ZW1cbn07XG4iXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6Ijs7Ozs7Ozs7QUFJQSxNQUFNLE9BQU4sQ0FBYTtnQkFDRTtTQUNOLFFBQUwsR0FBZ0IsRUFBaEI7OztVQUdNLE9BQVIsRUFBZ0I7U0FDVCxRQUFMLENBQWMsSUFBZCxDQUFtQixPQUFuQjtXQUNPLE9BQVA7OztRQUdHO1dBQ0ksS0FBSyxRQUFaOzs7WUFH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