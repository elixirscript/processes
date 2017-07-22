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

/* @flow */

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

/* @flow */
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
//# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicHJvY2Vzc2VzLmpzIiwic291cmNlcyI6WyIuLi9zcmMvcHJvY2Vzc2VzL21haWxib3guanMiLCIuLi9zcmMvcHJvY2Vzc2VzL3N0YXRlcy5qcyIsIi4uL3NyYy9wcm9jZXNzZXMvcHJvY2Vzcy5qcyIsIi4uL3NyYy9wcm9jZXNzZXMvc2NoZWR1bGVyLmpzIiwiLi4vc3JjL3Byb2Nlc3Nlcy9wcm9jZXNzX3N5c3RlbS5qcyIsIi4uL3NyYy9pbmRleC5qcyJdLCJzb3VyY2VzQ29udGVudCI6WyJcInVzZSBzdHJpY3RcIjtcblxuLyogQGZsb3cgKi9cblxuY2xhc3MgTWFpbGJveHtcbiAgY29uc3RydWN0b3IoKXtcbiAgICB0aGlzLm1lc3NhZ2VzID0gW107XG4gIH1cblxuICBkZWxpdmVyKG1lc3NhZ2Upe1xuICAgIHRoaXMubWVzc2FnZXMucHVzaChtZXNzYWdlKTtcbiAgICByZXR1cm4gbWVzc2FnZTtcbiAgfVxuXG4gIGdldCgpe1xuICAgIHJldHVybiB0aGlzLm1lc3NhZ2VzO1xuICB9XG5cbiAgaXNFbXB0eSgpe1xuICAgIHJldHVybiB0aGlzLm1lc3NhZ2VzLmxlbmd0aCA9PT0gMDtcbiAgfVxuXG4gIHJlbW92ZUF0KGluZGV4KXtcbiAgICB0aGlzLm1lc3NhZ2VzLnNwbGljZShpbmRleCwgMSk7XG4gIH1cbn1cblxuZXhwb3J0IGRlZmF1bHQgTWFpbGJveDtcbiIsImV4cG9ydCBkZWZhdWx0IHtcbiAgTk9STUFMOiBTeW1ib2wuZm9yKFwibm9ybWFsXCIpLFxuICBLSUxMOiBTeW1ib2wuZm9yKFwia2lsbFwiKSxcbiAgU1VTUEVORDogU3ltYm9sLmZvcihcInN1c3BlbmRcIiksXG4gIENPTlRJTlVFOiBTeW1ib2wuZm9yKFwiY29udGludWVcIiksXG4gIFJFQ0VJVkU6IFN5bWJvbC5mb3IoXCJyZWNlaXZlXCIpLFxuICBTRU5EOiBTeW1ib2wuZm9yKFwic2VuZFwiKSxcbiAgU0xFRVBJTkc6IFN5bWJvbC5mb3IoXCJzbGVlcGluZ1wiKSxcbiAgUlVOTklORzogU3ltYm9sLmZvcihcInJ1bm5pbmdcIiksXG4gIFNVU1BFTkRFRDogU3ltYm9sLmZvcihcInN1c3BlbmRlZFwiKSxcbiAgU1RPUFBFRDogU3ltYm9sLmZvcihcInN0b3BwZWRcIiksXG4gIFNMRUVQOiBTeW1ib2wuZm9yKFwic2xlZXBcIiksXG4gIEVYSVQ6IFN5bWJvbC5mb3IoXCJleGl0XCIpLFxuICBOT01BVENIOiBTeW1ib2wuZm9yKFwibm9fbWF0Y2hcIilcbn0iLCJcInVzZSBzdHJpY3RcIjtcblxuLyogQGZsb3cgKi9cbmltcG9ydCBNYWlsYm94IGZyb20gXCIuL21haWxib3hcIjtcbmltcG9ydCBQcm9jZXNzU3lzdGVtIGZyb20gXCIuL3Byb2Nlc3Nfc3lzdGVtXCI7XG5pbXBvcnQgU3RhdGVzIGZyb20gXCIuL3N0YXRlc1wiO1xuXG5mdW5jdGlvbiBpc19zbGVlcCh2YWx1ZSl7XG4gIHJldHVybiBBcnJheS5pc0FycmF5KHZhbHVlKSAmJiB2YWx1ZVswXSA9PT0gU3RhdGVzLlNMRUVQO1xufVxuXG5mdW5jdGlvbiBpc19yZWNlaXZlKHZhbHVlKXtcbiAgcmV0dXJuIEFycmF5LmlzQXJyYXkodmFsdWUpICYmIHZhbHVlWzBdID09PSBTdGF0ZXMuUkVDRUlWRTtcbn1cblxuZnVuY3Rpb24gcmVjZWl2ZV90aW1lZF9vdXQodmFsdWUpe1xuICByZXR1cm4gdmFsdWVbMl0gIT0gbnVsbCAmJiB2YWx1ZVsyXSA8IERhdGUubm93KCk7XG59XG5cbmNsYXNzIFByb2Nlc3Mge1xuICBjb25zdHJ1Y3RvcihwaWQsIGZ1bmMsIGFyZ3MsIG1haWxib3gsIHN5c3RlbSl7XG4gICAgdGhpcy5waWQgPSBwaWQ7XG4gICAgdGhpcy5mdW5jID0gZnVuYztcbiAgICB0aGlzLmFyZ3MgPSBhcmdzO1xuICAgIHRoaXMubWFpbGJveCA9IG1haWxib3g7XG4gICAgdGhpcy5zeXN0ZW0gPSBzeXN0ZW07XG4gICAgdGhpcy5zdGF0dXMgPSBTdGF0ZXMuU1RPUFBFRDtcbiAgICB0aGlzLmRpY3QgPSB7fTtcbiAgICB0aGlzLmZsYWdzID0ge307XG4gICAgdGhpcy5tb25pdG9ycyA9IFtdO1xuICB9XG5cbiAgc3RhcnQoKXtcbiAgICBjb25zdCBmdW5jdGlvbl9zY29wZSA9IHRoaXM7XG4gICAgbGV0IG1hY2hpbmUgPSB0aGlzLm1haW4oKTtcblxuICAgIHRoaXMuc3lzdGVtLnNjaGVkdWxlKGZ1bmN0aW9uKCkge1xuICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgbWFjaGluZS5uZXh0KCkpO1xuICAgIH0sIHRoaXMucGlkKTtcbiAgfVxuXG4gICptYWluKCkge1xuICAgIGxldCByZXR2YWwgPSBTdGF0ZXMuTk9STUFMO1xuXG4gICAgdHJ5IHtcbiAgICAgIHlpZWxkKiB0aGlzLmZ1bmMuYXBwbHkobnVsbCwgdGhpcy5hcmdzKTtcbiAgICB9IGNhdGNoKGUpIHtcbiAgICAgIGNvbnNvbGUuZXJyb3IoZSk7XG4gICAgICByZXR2YWwgPSBlO1xuICAgIH1cblxuICAgIHRoaXMuc3lzdGVtLmV4aXQocmV0dmFsKTtcbiAgfVxuXG4gIHByb2Nlc3NfZmxhZyhmbGFnLCB2YWx1ZSl7XG4gICAgY29uc3Qgb2xkX3ZhbHVlID0gdGhpcy5mbGFnc1tmbGFnXTtcbiAgICB0aGlzLmZsYWdzW2ZsYWddID0gdmFsdWU7XG4gICAgcmV0dXJuIG9sZF92YWx1ZTtcbiAgfVxuXG4gIGlzX3RyYXBwaW5nX2V4aXRzKCl7XG4gICAgcmV0dXJuIHRoaXMuZmxhZ3NbU3ltYm9sLmZvcihcInRyYXBfZXhpdFwiKV0gJiYgdGhpcy5mbGFnc1tTeW1ib2wuZm9yKFwidHJhcF9leGl0XCIpXSA9PSB0cnVlO1xuICB9XG5cbiAgc2lnbmFsKHJlYXNvbil7XG4gICAgaWYocmVhc29uICE9PSBTdGF0ZXMuTk9STUFMKXtcbiAgICAgIGNvbnNvbGUuZXJyb3IocmVhc29uKTtcbiAgICB9XG5cbiAgICB0aGlzLnN5c3RlbS5yZW1vdmVfcHJvYyh0aGlzLnBpZCwgcmVhc29uKTtcbiAgfVxuXG4gIHJlY2VpdmUoZnVuKXtcbiAgICBsZXQgdmFsdWUgPSBTdGF0ZXMuTk9NQVRDSDtcbiAgICBsZXQgbWVzc2FnZXMgPSB0aGlzLm1haWxib3guZ2V0KCk7XG5cbiAgICBmb3IobGV0IGkgPSAwOyBpIDwgbWVzc2FnZXMubGVuZ3RoOyBpKyspe1xuICAgICAgdHJ5e1xuICAgICAgICB2YWx1ZSA9IGZ1bihtZXNzYWdlc1tpXSk7XG4gICAgICAgIGlmKHZhbHVlICE9PSBTdGF0ZXMuTk9NQVRDSCl7XG4gICAgICAgICAgdGhpcy5tYWlsYm94LnJlbW92ZUF0KGkpO1xuICAgICAgICAgIGJyZWFrO1xuICAgICAgICB9XG4gICAgICB9Y2F0Y2goZSl7XG4gICAgICAgIGlmKGUuY29uc3RydWN0b3IubmFtZSAhPSBcIk1hdGNoRXJyb3JcIil7XG4gICAgICAgICAgdGhpcy5leGl0KGUpO1xuICAgICAgICB9XG4gICAgICB9XG4gICAgfVxuXG4gICAgcmV0dXJuIHZhbHVlO1xuICB9XG5cbiAgcnVuKG1hY2hpbmUsIHN0ZXApe1xuICAgIGNvbnN0IGZ1bmN0aW9uX3Njb3BlID0gdGhpcztcblxuICAgIGlmKCFzdGVwLmRvbmUpe1xuICAgICAgbGV0IHZhbHVlID0gc3RlcC52YWx1ZTtcblxuICAgICAgaWYoaXNfc2xlZXAodmFsdWUpKXtcblxuICAgICAgICB0aGlzLnN5c3RlbS5kZWxheShmdW5jdGlvbigpIHtcbiAgICAgICAgICBmdW5jdGlvbl9zY29wZS5zeXN0ZW0uc2V0X2N1cnJlbnQoZnVuY3Rpb25fc2NvcGUucGlkKTtcbiAgICAgICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgbWFjaGluZS5uZXh0KCkpO1xuICAgICAgICB9LCB2YWx1ZVsxXSk7XG5cbiAgICAgIH1lbHNlIGlmKGlzX3JlY2VpdmUodmFsdWUpICYmIHJlY2VpdmVfdGltZWRfb3V0KHZhbHVlKSl7XG5cbiAgICAgICAgbGV0IHJlc3VsdCA9IHZhbHVlWzNdKCk7XG5cbiAgICAgICAgdGhpcy5zeXN0ZW0uc2NoZWR1bGUoZnVuY3Rpb24oKSB7XG4gICAgICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICAgICAgZnVuY3Rpb25fc2NvcGUucnVuKG1hY2hpbmUsIG1hY2hpbmUubmV4dChyZXN1bHQpKTtcbiAgICAgICAgfSk7XG5cbiAgICAgIH1lbHNlIGlmKGlzX3JlY2VpdmUodmFsdWUpKXtcblxuICAgICAgICBsZXQgcmVzdWx0ID0gZnVuY3Rpb25fc2NvcGUucmVjZWl2ZSh2YWx1ZVsxXSk7XG5cbiAgICAgICAgaWYocmVzdWx0ID09PSBTdGF0ZXMuTk9NQVRDSCl7XG4gICAgICAgICAgdGhpcy5zeXN0ZW0uc3VzcGVuZChmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnN5c3RlbS5zZXRfY3VycmVudChmdW5jdGlvbl9zY29wZS5waWQpO1xuICAgICAgICAgICAgZnVuY3Rpb25fc2NvcGUucnVuKG1hY2hpbmUsIHN0ZXApO1xuICAgICAgICAgIH0pO1xuICAgICAgICB9ZWxzZXtcbiAgICAgICAgICB0aGlzLnN5c3RlbS5zY2hlZHVsZShmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnN5c3RlbS5zZXRfY3VycmVudChmdW5jdGlvbl9zY29wZS5waWQpO1xuICAgICAgICAgICAgZnVuY3Rpb25fc2NvcGUucnVuKG1hY2hpbmUsIG1hY2hpbmUubmV4dChyZXN1bHQpKTtcbiAgICAgICAgICB9KTtcbiAgICAgICAgfVxuXG4gICAgICB9ZWxzZXtcbiAgICAgICAgdGhpcy5zeXN0ZW0uc2NoZWR1bGUoZnVuY3Rpb24oKSB7XG4gICAgICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICAgICAgZnVuY3Rpb25fc2NvcGUucnVuKG1hY2hpbmUsIG1hY2hpbmUubmV4dCh2YWx1ZSkpO1xuICAgICAgICB9KTtcbiAgICAgIH1cbiAgICB9XG4gIH1cbn1cblxuZXhwb3J0IGRlZmF1bHQgUHJvY2VzcztcbiIsIlwidXNlIHN0cmljdFwiO1xuXG5jbGFzcyBQcm9jZXNzUXVldWUge1xuICBjb25zdHJ1Y3RvcihwaWQpe1xuICAgIHRoaXMucGlkID0gcGlkO1xuICAgIHRoaXMudGFza3MgPSBbXTtcbiAgfVxuXG4gIGVtcHR5KCl7XG4gICAgcmV0dXJuIHRoaXMudGFza3MubGVuZ3RoID09PSAwO1xuICB9XG5cbiAgYWRkKHRhc2spe1xuICAgIHRoaXMudGFza3MucHVzaCh0YXNrKTtcbiAgfVxuXG4gIG5leHQoKXtcbiAgICByZXR1cm4gdGhpcy50YXNrcy5zaGlmdCgpO1xuICB9XG59XG5cbmNsYXNzIFNjaGVkdWxlciB7XG4gICAgY29uc3RydWN0b3IodGhyb3R0bGUgPSAwLCByZWR1Y3Rpb25zX3Blcl9wcm9jZXNzID0gOCl7XG4gICAgICAgIHRoaXMuaXNSdW5uaW5nID0gZmFsc2U7XG4gICAgICAgIHRoaXMuaW52b2tlTGF0ZXIgPSBmdW5jdGlvbiAoY2FsbGJhY2spIHsgc2V0VGltZW91dChjYWxsYmFjaywgdGhyb3R0bGUpOyB9O1xuXG4gICAgICAgIC8vIEluIG91ciBjYXNlIGEgcmVkdWN0aW9uIGlzIGVxdWFsIHRvIGEgdGFzayBjYWxsXG4gICAgICAgIC8vIENvbnRyb2xzIGhvdyBtYW55IHRhc2tzIGFyZSBjYWxsZWQgYXQgYSB0aW1lIHBlciBwcm9jZXNzXG4gICAgICAgIHRoaXMucmVkdWN0aW9uc19wZXJfcHJvY2VzcyA9IHJlZHVjdGlvbnNfcGVyX3Byb2Nlc3M7XG4gICAgICAgIHRoaXMucXVldWVzID0gbmV3IE1hcCgpO1xuICAgICAgICB0aGlzLnJ1bigpO1xuICB9XG5cbiAgYWRkVG9RdWV1ZShwaWQsIHRhc2spe1xuICAgIGlmKCF0aGlzLnF1ZXVlcy5oYXMocGlkKSl7XG4gICAgICB0aGlzLnF1ZXVlcy5zZXQocGlkLCBuZXcgUHJvY2Vzc1F1ZXVlKHBpZCkpO1xuICAgIH1cblxuICAgIHRoaXMucXVldWVzLmdldChwaWQpLmFkZCh0YXNrKTtcbiAgfVxuXG4gIHJlbW92ZVBpZChwaWQpe1xuICAgIHRoaXMuaXNSdW5uaW5nID0gdHJ1ZTtcblxuICAgIHRoaXMucXVldWVzLmRlbGV0ZShwaWQpO1xuXG4gICAgdGhpcy5pc1J1bm5pbmcgPSBmYWxzZTtcbiAgfVxuXG4gIHJ1bigpe1xuICAgIGlmICh0aGlzLmlzUnVubmluZykge1xuICAgICAgdGhpcy5pbnZva2VMYXRlcigoKSA9PiB7IHRoaXMucnVuKCk7IH0pO1xuICAgIH0gZWxzZSB7XG4gICAgICBmb3IobGV0IFtwaWQsIHF1ZXVlXSBvZiB0aGlzLnF1ZXVlcyl7XG4gICAgICAgIGxldCByZWR1Y3Rpb25zID0gMDtcbiAgICAgICAgd2hpbGUocXVldWUgJiYgIXF1ZXVlLmVtcHR5KCkgJiYgcmVkdWN0aW9ucyA8IHRoaXMucmVkdWN0aW9uc19wZXJfcHJvY2Vzcyl7XG4gICAgICAgICAgbGV0IHRhc2sgPSBxdWV1ZS5uZXh0KCk7XG4gICAgICAgICAgdGhpcy5pc1J1bm5pbmcgPSB0cnVlO1xuXG4gICAgICAgICAgbGV0IHJlc3VsdDtcblxuICAgICAgICAgIHRyeXtcbiAgICAgICAgICAgIHJlc3VsdCA9IHRhc2soKTtcbiAgICAgICAgICB9Y2F0Y2goZSl7XG4gICAgICAgICAgICBjb25zb2xlLmVycm9yKGUpO1xuICAgICAgICAgICAgcmVzdWx0ID0gZTtcbiAgICAgICAgICB9XG5cbiAgICAgICAgICB0aGlzLmlzUnVubmluZyA9IGZhbHNlO1xuXG4gICAgICAgICAgaWYgKHJlc3VsdCBpbnN0YW5jZW9mIEVycm9yKSB7XG4gICAgICAgICAgICB0aHJvdyByZXN1bHQ7XG4gICAgICAgICAgfVxuXG4gICAgICAgICAgcmVkdWN0aW9ucysrO1xuICAgICAgICB9XG4gICAgICB9XG5cbiAgICAgIHRoaXMuaW52b2tlTGF0ZXIoKCkgPT4geyB0aGlzLnJ1bigpOyB9KTtcbiAgICB9XG4gIH1cblxuICBhZGRUb1NjaGVkdWxlcihwaWQsIHRhc2ssIGR1ZVRpbWUgPSAwKSB7XG4gICAgaWYoZHVlVGltZSA9PT0gMCl7XG4gICAgICB0aGlzLmludm9rZUxhdGVyKCgpID0+IHtcbiAgICAgICAgdGhpcy5hZGRUb1F1ZXVlKHBpZCwgdGFzayk7XG4gICAgICB9KTtcbiAgICB9ZWxzZXtcbiAgICAgIHNldFRpbWVvdXQoKCkgPT4ge1xuICAgICAgICB0aGlzLmFkZFRvUXVldWUocGlkLCB0YXNrKTtcbiAgICAgIH0sIGR1ZVRpbWUpO1xuICAgIH1cbiAgfTtcblxuICBzY2hlZHVsZShwaWQsIHRhc2spe1xuICAgIHRoaXMuYWRkVG9TY2hlZHVsZXIocGlkLCAoKSA9PiB7IHRhc2soKTsgfSk7XG4gIH1cblxuICBzY2hlZHVsZUZ1dHVyZShwaWQsIGR1ZVRpbWUsIHRhc2spe1xuICAgIHRoaXMuYWRkVG9TY2hlZHVsZXIocGlkLCAoKSA9PiB7IHRhc2soKTsgfSwgZHVlVGltZSk7XG4gIH1cbn1cblxuZXhwb3J0IGRlZmF1bHQgU2NoZWR1bGVyO1xuIiwiLyogQGZsb3cgKi9cblwidXNlIHN0cmljdFwiO1xuXG5pbXBvcnQgTWFpbGJveCBmcm9tIFwiLi9tYWlsYm94XCI7XG5pbXBvcnQgUHJvY2VzcyBmcm9tIFwiLi9wcm9jZXNzXCI7XG5pbXBvcnQgU3RhdGVzIGZyb20gXCIuL3N0YXRlc1wiO1xuaW1wb3J0IFNjaGVkdWxlciBmcm9tIFwiLi9zY2hlZHVsZXJcIjtcbmltcG9ydCBFcmxhbmdUeXBlcyBmcm9tIFwiZXJsYW5nLXR5cGVzXCI7XG5cblxuY2xhc3MgUHJvY2Vzc1N5c3RlbSB7XG5cbiAgY29uc3RydWN0b3IoKXtcbiAgICB0aGlzLnBpZHMgPSBuZXcgTWFwKCk7XG4gICAgdGhpcy5tYWlsYm94ZXMgPSBuZXcgTWFwKCk7XG4gICAgdGhpcy5uYW1lcyA9IG5ldyBNYXAoKTtcbiAgICB0aGlzLmxpbmtzID0gbmV3IE1hcCgpO1xuICAgIHRoaXMubW9uaXRvcnMgPSBuZXcgTWFwKCk7XG5cbiAgICBjb25zdCB0aHJvdHRsZSA9IDU7IC8vbXMgYmV0d2VlbiBzY2hlZHVsZWQgdGFza3NcbiAgICB0aGlzLmN1cnJlbnRfcHJvY2VzcyA9IG51bGw7XG4gICAgdGhpcy5zY2hlZHVsZXIgPSBuZXcgU2NoZWR1bGVyKHRocm90dGxlKTtcbiAgICB0aGlzLnN1c3BlbmRlZCA9IG5ldyBNYXAoKTtcblxuICAgIGxldCBwcm9jZXNzX3N5c3RlbV9zY29wZSA9IHRoaXM7XG4gICAgdGhpcy5tYWluX3Byb2Nlc3NfcGlkID0gdGhpcy5zcGF3bihmdW5jdGlvbiooKXtcbiAgICAgIHlpZWxkIHByb2Nlc3Nfc3lzdGVtX3Njb3BlLnNsZWVwKFN5bWJvbC5mb3IoXCJJbmZpbml0eVwiKSk7XG4gICAgfSk7XG4gICAgdGhpcy5zZXRfY3VycmVudCh0aGlzLm1haW5fcHJvY2Vzc19waWQpO1xuICB9XG5cbiAgc3RhdGljICogcnVuKGZ1biwgYXJncywgY29udGV4dCA9IG51bGwpe1xuICAgIGlmKGZ1bi5jb25zdHJ1Y3Rvci5uYW1lID09PSBcIkdlbmVyYXRvckZ1bmN0aW9uXCIpe1xuICAgICAgcmV0dXJuIHlpZWxkKiBmdW4uYXBwbHkoY29udGV4dCwgYXJncyk7XG4gICAgfWVsc2V7XG4gICAgICByZXR1cm4geWllbGQgZnVuLmFwcGx5KGNvbnRleHQsIGFyZ3MpO1xuICAgIH1cbiAgfVxuXG4gIHNwYXduKC4uLmFyZ3Mpe1xuICAgIGlmKGFyZ3MubGVuZ3RoID09PSAxKXtcbiAgICAgIGxldCBmdW4gPSBhcmdzWzBdO1xuICAgICAgcmV0dXJuIHRoaXMuYWRkX3Byb2MoZnVuLCBbXSwgZmFsc2UpLnBpZDtcblxuICAgIH1lbHNle1xuICAgICAgbGV0IG1vZCA9IGFyZ3NbMF07XG4gICAgICBsZXQgZnVuID0gYXJnc1sxXTtcbiAgICAgIGxldCB0aGVfYXJncyA9IGFyZ3NbMl07XG5cbiAgICAgIHJldHVybiB0aGlzLmFkZF9wcm9jKG1vZFtmdW5dLCB0aGVfYXJncywgZmFsc2UsIGZhbHNlKS5waWQ7XG4gICAgfVxuICB9XG5cbiAgc3Bhd25fbGluayguLi5hcmdzKXtcbiAgICBpZihhcmdzLmxlbmd0aCA9PT0gMSl7XG4gICAgICBsZXQgZnVuID0gYXJnc1swXTtcbiAgICAgIHJldHVybiB0aGlzLmFkZF9wcm9jKGZ1biwgW10sIHRydWUsIGZhbHNlKS5waWQ7XG5cbiAgICB9ZWxzZXtcbiAgICAgIGxldCBtb2QgPSBhcmdzWzBdO1xuICAgICAgbGV0IGZ1biA9IGFyZ3NbMV07XG4gICAgICBsZXQgdGhlX2FyZ3MgPSBhcmdzWzJdO1xuXG4gICAgICByZXR1cm4gdGhpcy5hZGRfcHJvYyhtb2RbZnVuXSwgdGhlX2FyZ3MsIHRydWUsIGZhbHNlKS5waWQ7XG4gICAgfVxuICB9XG5cbiAgbGluayhwaWQpe1xuICAgIHRoaXMubGlua3MuZ2V0KHRoaXMucGlkKCkpLmFkZChwaWQpO1xuICAgIHRoaXMubGlua3MuZ2V0KHBpZCkuYWRkKHRoaXMucGlkKCkpO1xuICB9XG5cbiAgdW5saW5rKHBpZCl7XG4gICAgdGhpcy5saW5rcy5nZXQodGhpcy5waWQoKSkuZGVsZXRlKHBpZCk7XG4gICAgdGhpcy5saW5rcy5nZXQocGlkKS5kZWxldGUodGhpcy5waWQoKSk7XG4gIH1cblxuICBzcGF3bl9tb25pdG9yKC4uLmFyZ3Mpe1xuICAgIGlmKGFyZ3MubGVuZ3RoID09PSAxKXtcbiAgICAgIGxldCBmdW4gPSBhcmdzWzBdO1xuICAgICAgbGV0IHByb2Nlc3MgPSB0aGlzLmFkZF9wcm9jKGZ1biwgW10sIGZhbHNlLCB0cnVlKTtcbiAgICAgIHJldHVybiBbcHJvY2Vzcy5waWQsIHByb2Nlc3MubW9uaXRvcnNbMF1dO1xuXG4gICAgfWVsc2V7XG4gICAgICBsZXQgbW9kID0gYXJnc1swXTtcbiAgICAgIGxldCBmdW4gPSBhcmdzWzFdO1xuICAgICAgbGV0IHRoZV9hcmdzID0gYXJnc1syXTtcbiAgICAgIGxldCBwcm9jZXNzID0gdGhpcy5hZGRfcHJvYyhtb2RbZnVuXSwgdGhlX2FyZ3MsIGZhbHNlLCB0cnVlKTtcblxuICAgICAgcmV0dXJuIFtwcm9jZXNzLnBpZCwgcHJvY2Vzcy5tb25pdG9yc1swXV07XG4gICAgfVxuICB9XG5cbiAgbW9uaXRvcihwaWQpe1xuICAgIGNvbnN0IHJlYWxfcGlkID0gdGhpcy5waWRvZihwaWQpO1xuICAgIGNvbnN0IHJlZiA9IHRoaXMubWFrZV9yZWYoKTtcblxuICAgIGlmKHJlYWxfcGlkKXtcblxuICAgICAgdGhpcy5tb25pdG9ycy5zZXQocmVmLCB7J21vbml0b3InOiB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5waWQsICdtb25pdGVlJzogcmVhbF9waWR9KTtcbiAgICAgIHRoaXMucGlkcy5nZXQocmVhbF9waWQpLm1vbml0b3JzLnB1c2gocmVmKTtcbiAgICAgIHJldHVybiByZWY7XG4gICAgfWVsc2V7XG4gICAgICB0aGlzLnNlbmQodGhpcy5jdXJyZW50X3Byb2Nlc3MucGlkLCBuZXcgRXJsYW5nVHlwZXMuVHVwbGUoJ0RPV04nLCByZWYsIHBpZCwgcmVhbF9waWQsIFN5bWJvbC5mb3IoJ25vcHJvYycpKSk7XG4gICAgICByZXR1cm4gcmVmO1xuICAgIH1cbiAgfVxuXG4gIGRlbW9uaXRvcihyZWYpe1xuICAgIGlmKHRoaXMubW9uaXRvci5oYXMocmVmKSl7XG4gICAgICB0aGlzLm1vbml0b3IuZGVsZXRlKHJlZik7XG4gICAgICByZXR1cm4gdHJ1ZTtcbiAgICB9XG5cbiAgICByZXR1cm4gZmFsc2U7XG4gIH1cblxuICBzZXRfY3VycmVudChpZCl7XG4gICAgbGV0IHBpZCA9IHRoaXMucGlkb2YoaWQpO1xuICAgIGlmKHBpZCAhPT0gbnVsbCl7XG4gICAgICB0aGlzLmN1cnJlbnRfcHJvY2VzcyA9IHRoaXMucGlkcy5nZXQocGlkKTtcbiAgICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLnN0YXR1cyA9IFN0YXRlcy5SVU5OSU5HO1xuICAgIH1cbiAgfVxuXG4gIGFkZF9wcm9jKGZ1biwgYXJncywgbGlua2VkLCBtb25pdG9yZWQpe1xuICAgIGxldCBuZXdwaWQgPSBuZXcgRXJsYW5nVHlwZXMuUElEKCk7XG4gICAgbGV0IG1haWxib3ggPSBuZXcgTWFpbGJveCgpO1xuICAgIGxldCBuZXdwcm9jID0gbmV3IFByb2Nlc3MobmV3cGlkLCBmdW4sIGFyZ3MsIG1haWxib3gsIHRoaXMpO1xuXG4gICAgdGhpcy5waWRzLnNldChuZXdwaWQsIG5ld3Byb2MpO1xuICAgIHRoaXMubWFpbGJveGVzLnNldChuZXdwaWQsIG1haWxib3gpO1xuICAgIHRoaXMubGlua3Muc2V0KG5ld3BpZCwgbmV3IFNldCgpKTtcblxuICAgIGlmKGxpbmtlZCl7XG4gICAgICB0aGlzLmxpbmsobmV3cGlkKTtcbiAgICB9XG5cbiAgICBpZihtb25pdG9yZWQpe1xuICAgICAgdGhpcy5tb25pdG9yKG5ld3BpZCk7XG4gICAgfVxuXG4gICAgbmV3cHJvYy5zdGFydCgpO1xuICAgIHJldHVybiBuZXdwcm9jO1xuICB9XG5cbiAgcmVtb3ZlX3Byb2MocGlkLCBleGl0cmVhc29uKXtcbiAgICB0aGlzLnBpZHMuZGVsZXRlKHBpZCk7XG4gICAgdGhpcy51bnJlZ2lzdGVyKHBpZCk7XG4gICAgdGhpcy5zY2hlZHVsZXIucmVtb3ZlUGlkKHBpZCk7XG5cbiAgICBpZih0aGlzLmxpbmtzLmhhcyhwaWQpKXtcbiAgICAgIGZvciAobGV0IGxpbmtwaWQgb2YgdGhpcy5saW5rcy5nZXQocGlkKSkge1xuICAgICAgICB0aGlzLmV4aXQobGlua3BpZCwgZXhpdHJlYXNvbik7XG4gICAgICAgIHRoaXMubGlua3MuZ2V0KGxpbmtwaWQpLmRlbGV0ZShwaWQpO1xuICAgICAgfVxuXG4gICAgICB0aGlzLmxpbmtzLmRlbGV0ZShwaWQpO1xuICAgIH1cbiAgfVxuXG4gIHJlZ2lzdGVyKG5hbWUsIHBpZCl7XG4gICAgaWYoIXRoaXMubmFtZXMuaGFzKG5hbWUpKXtcbiAgICAgIHRoaXMubmFtZXMuc2V0KG5hbWUsIHBpZCk7XG4gICAgfWVsc2V7XG4gICAgICB0aHJvdyBuZXcgRXJyb3IoXCJOYW1lIGlzIGFscmVhZHkgcmVnaXN0ZXJlZCB0byBhbm90aGVyIHByb2Nlc3NcIik7XG4gICAgfVxuICB9XG5cbiAgd2hlcmVpcyhuYW1lKXtcbiAgICByZXR1cm4gdGhpcy5uYW1lcy5oYXMobmFtZSkgPyB0aGlzLm5hbWVzLmdldChuYW1lKSA6IG51bGw7XG4gIH1cblxuICByZWdpc3RlcmVkKCl7XG4gICAgcmV0dXJuIHRoaXMubmFtZXMua2V5cygpO1xuICB9XG5cbiAgdW5yZWdpc3RlcihwaWQpe1xuICAgIGZvcihsZXQgbmFtZSBvZiB0aGlzLm5hbWVzLmtleXMoKSl7XG4gICAgICBpZih0aGlzLm5hbWVzLmhhcyhuYW1lKSAmJiB0aGlzLm5hbWVzLmdldChuYW1lKSA9PT0gcGlkKXtcbiAgICAgICAgdGhpcy5uYW1lcy5kZWxldGUobmFtZSk7XG4gICAgICB9XG4gICAgfVxuICB9XG5cbiAgcGlkKCl7XG4gICAgcmV0dXJuIHRoaXMuY3VycmVudF9wcm9jZXNzLnBpZDtcbiAgfVxuXG4gIHBpZG9mKGlkKXtcbiAgICBpZiAoaWQgaW5zdGFuY2VvZiBFcmxhbmdUeXBlcy5QSUQpIHtcbiAgICAgICByZXR1cm4gdGhpcy5waWRzLmhhcyhpZCkgPyBpZCA6IG51bGw7XG4gICAgfSBlbHNlIGlmIChpZCBpbnN0YW5jZW9mIFByb2Nlc3MpIHtcbiAgICAgICByZXR1cm4gaWQucGlkO1xuICAgIH0gZWxzZSB7XG4gICAgICAgbGV0IHBpZCA9IHRoaXMud2hlcmVpcyhpZCk7XG4gICAgICAgaWYgKHBpZCA9PT0gbnVsbClcbiAgICAgICAgICB0aHJvdyhcIlByb2Nlc3MgbmFtZSBub3QgcmVnaXN0ZXJlZDogXCIgKyBpZCArIFwiIChcIiArIHR5cGVvZihpZCkgKyBcIilcIik7XG4gICAgICAgcmV0dXJuIHBpZDtcbiAgICB9XG4gIH1cblxuICBzZW5kKGlkLCBtc2cpIHtcbiAgICBjb25zdCBwaWQgPSB0aGlzLnBpZG9mKGlkKTtcblxuICAgIGlmKHBpZCl7XG4gICAgICB0aGlzLm1haWxib3hlcy5nZXQocGlkKS5kZWxpdmVyKG1zZyk7XG5cbiAgICAgIGlmKHRoaXMuc3VzcGVuZGVkLmhhcyhwaWQpKXtcbiAgICAgICAgbGV0IGZ1biA9IHRoaXMuc3VzcGVuZGVkLmdldChwaWQpO1xuICAgICAgICB0aGlzLnN1c3BlbmRlZC5kZWxldGUocGlkKTtcbiAgICAgICAgdGhpcy5zY2hlZHVsZShmdW4pO1xuICAgICAgfVxuICAgIH1cblxuICAgIHJldHVybiBtc2c7XG4gIH1cblxuICByZWNlaXZlKGZ1biwgdGltZW91dCA9IDAsIHRpbWVvdXRGbiA9ICgpID0+IHRydWUgKSB7XG4gICAgbGV0IERhdGVUaW1lb3V0ID0gbnVsbDtcblxuICAgIGlmKHRpbWVvdXQgPT09IDAgfHwgdGltZW91dCA9PT0gSW5maW5pdHkpe1xuICAgICAgRGF0ZVRpbWVvdXQgPSBudWxsO1xuICAgIH1lbHNle1xuICAgICAgRGF0ZVRpbWVvdXQgPSBEYXRlLm5vdygpICsgdGltZW91dDtcbiAgICB9XG5cbiAgICByZXR1cm4gW1xuICAgICAgU3RhdGVzLlJFQ0VJVkUsXG4gICAgICBmdW4sXG4gICAgICBEYXRlVGltZW91dCxcbiAgICAgIHRpbWVvdXRGblxuICAgIF07XG4gIH1cblxuICBzbGVlcChkdXJhdGlvbil7XG4gICAgcmV0dXJuIFtTdGF0ZXMuU0xFRVAsIGR1cmF0aW9uXTtcbiAgfVxuXG4gIHN1c3BlbmQoZnVuKXtcbiAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5zdGF0dXMgPSBTdGF0ZXMuU1VTUEVOREVEO1xuICAgIHRoaXMuc3VzcGVuZGVkLnNldCh0aGlzLmN1cnJlbnRfcHJvY2Vzcy5waWQsIGZ1bik7XG4gIH1cblxuICBkZWxheShmdW4sIHRpbWUpe1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLnN0YXR1cyA9IFN0YXRlcy5TTEVFUElORztcblxuICAgIGlmKE51bWJlci5pc0ludGVnZXIodGltZSkpe1xuICAgICAgdGhpcy5zY2hlZHVsZXIuc2NoZWR1bGVGdXR1cmUodGhpcy5jdXJyZW50X3Byb2Nlc3MucGlkLCB0aW1lLCBmdW4pO1xuICAgIH1cbiAgfVxuXG4gIHNjaGVkdWxlKGZ1biwgcGlkKXtcbiAgICBjb25zdCB0aGVfcGlkID0gcGlkICE9IG51bGwgPyBwaWQgOiB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5waWQ7XG4gICAgdGhpcy5zY2hlZHVsZXIuc2NoZWR1bGUodGhlX3BpZCwgZnVuKTtcbiAgfVxuXG4gIGV4aXQob25lLCB0d28pe1xuICAgIGxldCBwaWQgPSBudWxsO1xuICAgIGxldCByZWFzb24gPSBudWxsO1xuICAgIGxldCBwcm9jZXNzID0gbnVsbDtcblxuICAgIGlmKHR3byl7XG4gICAgICBwaWQgPSBvbmU7XG4gICAgICByZWFzb24gPSB0d287XG4gICAgICBwcm9jZXNzID0gdGhpcy5waWRzLmdldCh0aGlzLnBpZG9mKHBpZCkpO1xuXG4gICAgICBpZigocHJvY2VzcyAmJiBwcm9jZXNzLmlzX3RyYXBwaW5nX2V4aXRzKCkpIHx8IHJlYXNvbiA9PT0gU3RhdGVzLktJTEwgfHwgcmVhc29uID09PSBTdGF0ZXMuTk9STUFMKXtcbiAgICAgICAgdGhpcy5tYWlsYm94ZXMuZ2V0KHByb2Nlc3MucGlkKS5kZWxpdmVyKG5ldyBFcmxhbmdUeXBlcy5UdXBsZShTdGF0ZXMuRVhJVCwgdGhpcy5waWQoKSwgcmVhc29uICkpO1xuICAgICAgfSBlbHNle1xuICAgICAgICBwcm9jZXNzLnNpZ25hbChyZWFzb24pO1xuICAgICAgfVxuXG4gICAgfWVsc2V7XG4gICAgICBwaWQgPSB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5waWQ7XG4gICAgICByZWFzb24gPSBvbmU7XG4gICAgICBwcm9jZXNzID0gdGhpcy5jdXJyZW50X3Byb2Nlc3M7XG5cbiAgICAgIHByb2Nlc3Muc2lnbmFsKHJlYXNvbik7XG4gICAgfVxuXG4gICAgZm9yKGxldCByZWYgaW4gcHJvY2Vzcy5tb25pdG9ycyl7XG4gICAgICBsZXQgbW9ucyA9IHRoaXMubW9uaXRvcnMuZ2V0KHJlZik7XG4gICAgICB0aGlzLnNlbmQobW9uc1snbW9uaXRvciddLCBuZXcgRXJsYW5nVHlwZXMuVHVwbGUoJ0RPV04nLCByZWYsIG1vbnNbJ21vbml0ZWUnXSwgbW9uc1snbW9uaXRlZSddLCByZWFzb24pKTtcbiAgICB9XG4gIH1cblxuICBlcnJvcihyZWFzb24pe1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLnNpZ25hbChyZWFzb24pO1xuICB9XG5cbiAgcHJvY2Vzc19mbGFnKC4uLmFyZ3Mpe1xuICAgIGlmKGFyZ3MubGVuZ3RoID09IDIpe1xuICAgICAgY29uc3QgZmxhZyA9IGFyZ3NbMF07XG4gICAgICBjb25zdCB2YWx1ZSA9IGFyZ3NbMV07XG4gICAgICByZXR1cm4gdGhpcy5jdXJyZW50X3Byb2Nlc3MucHJvY2Vzc19mbGFnKGZsYWcsIHZhbHVlKTtcbiAgICB9ZWxzZXtcbiAgICAgIGNvbnN0IHBpZCA9IHRoaXMucGlkb2YoYXJnc1swXSk7XG4gICAgICBjb25zdCBmbGFnID0gYXJnc1sxXTtcbiAgICAgIGNvbnN0IHZhbHVlID0gYXJnc1syXTtcbiAgICAgIHJldHVybiB0aGlzLnBpZHMuZ2V0KHBpZCkucHJvY2Vzc19mbGFnKGZsYWcsIHZhbHVlKTtcbiAgICB9XG4gIH1cblxuICBwdXQoa2V5LCB2YWx1ZSl7XG4gICAgdGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdFtrZXldID0gdmFsdWU7XG4gIH1cblxuICBnZXRfcHJvY2Vzc19kaWN0KCl7XG4gICAgcmV0dXJuIHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3Q7XG4gIH1cblxuICBnZXQoa2V5LCBkZWZhdWx0X3ZhbHVlID0gbnVsbCl7XG4gICAgaWYoa2V5IGluIHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3Qpe1xuICAgICAgcmV0dXJuIHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3Rba2V5XTtcbiAgICB9ZWxzZXtcbiAgICAgIHJldHVybiBkZWZhdWx0X3ZhbHVlO1xuICAgIH1cbiAgfVxuXG4gIGdldF9rZXlzKHZhbHVlKXtcbiAgICBpZih2YWx1ZSl7XG4gICAgICBsZXQga2V5cyA9IFtdO1xuXG4gICAgICBmb3IobGV0IGtleSBvZiBPYmplY3Qua2V5cyh0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0KSl7XG4gICAgICAgIGlmKHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3Rba2V5XSA9PT0gdmFsdWUpe1xuICAgICAgICAgIGtleXMucHVzaChrZXkpO1xuICAgICAgICB9XG4gICAgICB9XG5cbiAgICAgIHJldHVybiBrZXlzO1xuICAgIH1cblxuICAgIHJldHVybiBPYmplY3Qua2V5cyh0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0KTtcbiAgfVxuXG4gIGVyYXNlKGtleSl7XG4gICAgaWYoa2V5ICE9IG51bGwpe1xuICAgICAgZGVsZXRlIHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3Rba2V5XTtcbiAgICB9ZWxzZXtcbiAgICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3QgPSB7fTtcbiAgICB9XG4gIH1cblxuICBpc19hbGl2ZShwaWQpe1xuICAgIGNvbnN0IHJlYWxfcGlkID0gdGhpcy5waWRvZihwaWQpO1xuICAgIHJldHVybiByZWFsX3BpZCAhPSBudWxsO1xuICB9XG5cbiAgbGlzdCgpe1xuICAgIHJldHVybiBBcnJheS5mcm9tKHRoaXMucGlkcy5rZXlzKCkpO1xuICB9XG5cbiAgbWFrZV9yZWYoKXtcbiAgICByZXR1cm4gbmV3IEVybGFuZ1R5cGVzLlJlZmVyZW5jZSgpO1xuICB9XG59XG5cbmV4cG9ydCBkZWZhdWx0IFByb2Nlc3NTeXN0ZW07XG4iLCJpbXBvcnQgUHJvY2Vzc1N5c3RlbSBmcm9tIFwiLi9wcm9jZXNzZXMvcHJvY2Vzc19zeXN0ZW1cIjtcblxuZXhwb3J0IGRlZmF1bHQge1xuICBQcm9jZXNzU3lzdGVtXG59O1xuIl0sIm5hbWVzIjpbIk1haWxib3giLCJtZXNzYWdlcyIsIm1lc3NhZ2UiLCJwdXNoIiwibGVuZ3RoIiwiaW5kZXgiLCJzcGxpY2UiLCJTeW1ib2wiLCJmb3IiLCJpc19zbGVlcCIsInZhbHVlIiwiQXJyYXkiLCJpc0FycmF5IiwiU3RhdGVzIiwiU0xFRVAiLCJpc19yZWNlaXZlIiwiUkVDRUlWRSIsInJlY2VpdmVfdGltZWRfb3V0IiwiRGF0ZSIsIm5vdyIsIlByb2Nlc3MiLCJwaWQiLCJmdW5jIiwiYXJncyIsIm1haWxib3giLCJzeXN0ZW0iLCJzdGF0dXMiLCJTVE9QUEVEIiwiZGljdCIsImZsYWdzIiwibW9uaXRvcnMiLCJmdW5jdGlvbl9zY29wZSIsIm1hY2hpbmUiLCJtYWluIiwic2NoZWR1bGUiLCJzZXRfY3VycmVudCIsInJ1biIsIm5leHQiLCJyZXR2YWwiLCJOT1JNQUwiLCJhcHBseSIsImUiLCJlcnJvciIsImV4aXQiLCJmbGFnIiwib2xkX3ZhbHVlIiwicmVhc29uIiwicmVtb3ZlX3Byb2MiLCJmdW4iLCJOT01BVENIIiwiZ2V0IiwiaSIsInJlbW92ZUF0IiwiY29uc3RydWN0b3IiLCJuYW1lIiwic3RlcCIsImRvbmUiLCJkZWxheSIsInJlc3VsdCIsInJlY2VpdmUiLCJzdXNwZW5kIiwiUHJvY2Vzc1F1ZXVlIiwidGFza3MiLCJ0YXNrIiwic2hpZnQiLCJTY2hlZHVsZXIiLCJ0aHJvdHRsZSIsInJlZHVjdGlvbnNfcGVyX3Byb2Nlc3MiLCJpc1J1bm5pbmciLCJpbnZva2VMYXRlciIsImNhbGxiYWNrIiwicXVldWVzIiwiTWFwIiwiaGFzIiwic2V0IiwiYWRkIiwiZGVsZXRlIiwicXVldWUiLCJyZWR1Y3Rpb25zIiwiZW1wdHkiLCJFcnJvciIsImR1ZVRpbWUiLCJhZGRUb1F1ZXVlIiwiYWRkVG9TY2hlZHVsZXIiLCJQcm9jZXNzU3lzdGVtIiwicGlkcyIsIm1haWxib3hlcyIsIm5hbWVzIiwibGlua3MiLCJjdXJyZW50X3Byb2Nlc3MiLCJzY2hlZHVsZXIiLCJzdXNwZW5kZWQiLCJwcm9jZXNzX3N5c3RlbV9zY29wZSIsIm1haW5fcHJvY2Vzc19waWQiLCJzcGF3biIsInNsZWVwIiwiY29udGV4dCIsImFkZF9wcm9jIiwibW9kIiwidGhlX2FyZ3MiLCJwcm9jZXNzIiwicmVhbF9waWQiLCJwaWRvZiIsInJlZiIsIm1ha2VfcmVmIiwic2VuZCIsIkVybGFuZ1R5cGVzIiwiVHVwbGUiLCJtb25pdG9yIiwiaWQiLCJSVU5OSU5HIiwibGlua2VkIiwibW9uaXRvcmVkIiwibmV3cGlkIiwiUElEIiwibmV3cHJvYyIsIlNldCIsImxpbmsiLCJzdGFydCIsImV4aXRyZWFzb24iLCJ1bnJlZ2lzdGVyIiwicmVtb3ZlUGlkIiwibGlua3BpZCIsImtleXMiLCJ3aGVyZWlzIiwibXNnIiwiZGVsaXZlciIsInRpbWVvdXQiLCJ0aW1lb3V0Rm4iLCJEYXRlVGltZW91dCIsIkluZmluaXR5IiwiZHVyYXRpb24iLCJTVVNQRU5ERUQiLCJ0aW1lIiwiU0xFRVBJTkciLCJOdW1iZXIiLCJpc0ludGVnZXIiLCJzY2hlZHVsZUZ1dHVyZSIsInRoZV9waWQiLCJvbmUiLCJ0d28iLCJpc190cmFwcGluZ19leGl0cyIsIktJTEwiLCJFWElUIiwic2lnbmFsIiwibW9ucyIsInByb2Nlc3NfZmxhZyIsImtleSIsImRlZmF1bHRfdmFsdWUiLCJPYmplY3QiLCJmcm9tIiwiUmVmZXJlbmNlIl0sIm1hcHBpbmdzIjoiOzs7Ozs7QUFFQTs7QUFFQSxNQUFNQSxPQUFOLENBQWE7Z0JBQ0U7U0FDTkMsUUFBTCxHQUFnQixFQUFoQjs7O1VBR01DLE9BQVIsRUFBZ0I7U0FDVEQsUUFBTCxDQUFjRSxJQUFkLENBQW1CRCxPQUFuQjtXQUNPQSxPQUFQOzs7UUFHRztXQUNJLEtBQUtELFFBQVo7OztZQUdPO1dBQ0EsS0FBS0EsUUFBTCxDQUFjRyxNQUFkLEtBQXlCLENBQWhDOzs7V0FHT0MsS0FBVCxFQUFlO1NBQ1JKLFFBQUwsQ0FBY0ssTUFBZCxDQUFxQkQsS0FBckIsRUFBNEIsQ0FBNUI7Ozs7QUN2QkosYUFBZTtVQUNMRSxPQUFPQyxHQUFQLENBQVcsUUFBWCxDQURLO1FBRVBELE9BQU9DLEdBQVAsQ0FBVyxNQUFYLENBRk87V0FHSkQsT0FBT0MsR0FBUCxDQUFXLFNBQVgsQ0FISTtZQUlIRCxPQUFPQyxHQUFQLENBQVcsVUFBWCxDQUpHO1dBS0pELE9BQU9DLEdBQVAsQ0FBVyxTQUFYLENBTEk7UUFNUEQsT0FBT0MsR0FBUCxDQUFXLE1BQVgsQ0FOTztZQU9IRCxPQUFPQyxHQUFQLENBQVcsVUFBWCxDQVBHO1dBUUpELE9BQU9DLEdBQVAsQ0FBVyxTQUFYLENBUkk7YUFTRkQsT0FBT0MsR0FBUCxDQUFXLFdBQVgsQ0FURTtXQVVKRCxPQUFPQyxHQUFQLENBQVcsU0FBWCxDQVZJO1NBV05ELE9BQU9DLEdBQVAsQ0FBVyxPQUFYLENBWE07UUFZUEQsT0FBT0MsR0FBUCxDQUFXLE1BQVgsQ0FaTztXQWFKRCxPQUFPQyxHQUFQLENBQVcsVUFBWDtDQWJYOztBQ0VBOztBQUNBLEFBSUEsU0FBU0MsUUFBVCxDQUFrQkMsS0FBbEIsRUFBd0I7U0FDZkMsTUFBTUMsT0FBTixDQUFjRixLQUFkLEtBQXdCQSxNQUFNLENBQU4sTUFBYUcsT0FBT0MsS0FBbkQ7OztBQUdGLFNBQVNDLFVBQVQsQ0FBb0JMLEtBQXBCLEVBQTBCO1NBQ2pCQyxNQUFNQyxPQUFOLENBQWNGLEtBQWQsS0FBd0JBLE1BQU0sQ0FBTixNQUFhRyxPQUFPRyxPQUFuRDs7O0FBR0YsU0FBU0MsaUJBQVQsQ0FBMkJQLEtBQTNCLEVBQWlDO1NBQ3hCQSxNQUFNLENBQU4sS0FBWSxJQUFaLElBQW9CQSxNQUFNLENBQU4sSUFBV1EsS0FBS0MsR0FBTCxFQUF0Qzs7O0FBR0YsTUFBTUMsT0FBTixDQUFjO2NBQ0FDLEdBQVosRUFBaUJDLElBQWpCLEVBQXVCQyxJQUF2QixFQUE2QkMsT0FBN0IsRUFBc0NDLE1BQXRDLEVBQTZDO1NBQ3RDSixHQUFMLEdBQVdBLEdBQVg7U0FDS0MsSUFBTCxHQUFZQSxJQUFaO1NBQ0tDLElBQUwsR0FBWUEsSUFBWjtTQUNLQyxPQUFMLEdBQWVBLE9BQWY7U0FDS0MsTUFBTCxHQUFjQSxNQUFkO1NBQ0tDLE1BQUwsR0FBY2IsT0FBT2MsT0FBckI7U0FDS0MsSUFBTCxHQUFZLEVBQVo7U0FDS0MsS0FBTCxHQUFhLEVBQWI7U0FDS0MsUUFBTCxHQUFnQixFQUFoQjs7O1VBR0s7VUFDQ0MsaUJBQWlCLElBQXZCO1FBQ0lDLFVBQVUsS0FBS0MsSUFBTCxFQUFkOztTQUVLUixNQUFMLENBQVlTLFFBQVosQ0FBcUIsWUFBVztxQkFDZlQsTUFBZixDQUFzQlUsV0FBdEIsQ0FBa0NKLGVBQWVWLEdBQWpEO3FCQUNlZSxHQUFmLENBQW1CSixPQUFuQixFQUE0QkEsUUFBUUssSUFBUixFQUE1QjtLQUZGLEVBR0csS0FBS2hCLEdBSFI7OztHQU1EWSxJQUFELEdBQVE7UUFDRkssU0FBU3pCLE9BQU8wQixNQUFwQjs7UUFFSTthQUNLLEtBQUtqQixJQUFMLENBQVVrQixLQUFWLENBQWdCLElBQWhCLEVBQXNCLEtBQUtqQixJQUEzQixDQUFQO0tBREYsQ0FFRSxPQUFNa0IsQ0FBTixFQUFTO2NBQ0RDLEtBQVIsQ0FBY0QsQ0FBZDtlQUNTQSxDQUFUOzs7U0FHR2hCLE1BQUwsQ0FBWWtCLElBQVosQ0FBaUJMLE1BQWpCOzs7ZUFHV00sSUFBYixFQUFtQmxDLEtBQW5CLEVBQXlCO1VBQ2pCbUMsWUFBWSxLQUFLaEIsS0FBTCxDQUFXZSxJQUFYLENBQWxCO1NBQ0tmLEtBQUwsQ0FBV2UsSUFBWCxJQUFtQmxDLEtBQW5CO1dBQ09tQyxTQUFQOzs7c0JBR2lCO1dBQ1YsS0FBS2hCLEtBQUwsQ0FBV3RCLE9BQU9DLEdBQVAsQ0FBVyxXQUFYLENBQVgsS0FBdUMsS0FBS3FCLEtBQUwsQ0FBV3RCLE9BQU9DLEdBQVAsQ0FBVyxXQUFYLENBQVgsS0FBdUMsSUFBckY7OztTQUdLc0MsTUFBUCxFQUFjO1FBQ1RBLFdBQVdqQyxPQUFPMEIsTUFBckIsRUFBNEI7Y0FDbEJHLEtBQVIsQ0FBY0ksTUFBZDs7O1NBR0dyQixNQUFMLENBQVlzQixXQUFaLENBQXdCLEtBQUsxQixHQUE3QixFQUFrQ3lCLE1BQWxDOzs7VUFHTUUsR0FBUixFQUFZO1FBQ050QyxRQUFRRyxPQUFPb0MsT0FBbkI7UUFDSWhELFdBQVcsS0FBS3VCLE9BQUwsQ0FBYTBCLEdBQWIsRUFBZjs7U0FFSSxJQUFJQyxJQUFJLENBQVosRUFBZUEsSUFBSWxELFNBQVNHLE1BQTVCLEVBQW9DK0MsR0FBcEMsRUFBd0M7VUFDbkM7Z0JBQ09ILElBQUkvQyxTQUFTa0QsQ0FBVCxDQUFKLENBQVI7WUFDR3pDLFVBQVVHLE9BQU9vQyxPQUFwQixFQUE0QjtlQUNyQnpCLE9BQUwsQ0FBYTRCLFFBQWIsQ0FBc0JELENBQXRCOzs7T0FISixDQU1DLE9BQU1WLENBQU4sRUFBUTtZQUNKQSxFQUFFWSxXQUFGLENBQWNDLElBQWQsSUFBc0IsWUFBekIsRUFBc0M7ZUFDL0JYLElBQUwsQ0FBVUYsQ0FBVjs7Ozs7V0FLQy9CLEtBQVA7OztNQUdFc0IsT0FBSixFQUFhdUIsSUFBYixFQUFrQjtVQUNWeEIsaUJBQWlCLElBQXZCOztRQUVHLENBQUN3QixLQUFLQyxJQUFULEVBQWM7VUFDUjlDLFFBQVE2QyxLQUFLN0MsS0FBakI7O1VBRUdELFNBQVNDLEtBQVQsQ0FBSCxFQUFtQjs7YUFFWmUsTUFBTCxDQUFZZ0MsS0FBWixDQUFrQixZQUFXO3lCQUNaaEMsTUFBZixDQUFzQlUsV0FBdEIsQ0FBa0NKLGVBQWVWLEdBQWpEO3lCQUNlZSxHQUFmLENBQW1CSixPQUFuQixFQUE0QkEsUUFBUUssSUFBUixFQUE1QjtTQUZGLEVBR0czQixNQUFNLENBQU4sQ0FISDtPQUZGLE1BT00sSUFBR0ssV0FBV0wsS0FBWCxLQUFxQk8sa0JBQWtCUCxLQUFsQixDQUF4QixFQUFpRDs7WUFFakRnRCxTQUFTaEQsTUFBTSxDQUFOLEdBQWI7O2FBRUtlLE1BQUwsQ0FBWVMsUUFBWixDQUFxQixZQUFXO3lCQUNmVCxNQUFmLENBQXNCVSxXQUF0QixDQUFrQ0osZUFBZVYsR0FBakQ7eUJBQ2VlLEdBQWYsQ0FBbUJKLE9BQW5CLEVBQTRCQSxRQUFRSyxJQUFSLENBQWFxQixNQUFiLENBQTVCO1NBRkY7T0FKSSxNQVNBLElBQUczQyxXQUFXTCxLQUFYLENBQUgsRUFBcUI7O1lBRXJCZ0QsU0FBUzNCLGVBQWU0QixPQUFmLENBQXVCakQsTUFBTSxDQUFOLENBQXZCLENBQWI7O1lBRUdnRCxXQUFXN0MsT0FBT29DLE9BQXJCLEVBQTZCO2VBQ3RCeEIsTUFBTCxDQUFZbUMsT0FBWixDQUFvQixZQUFXOzJCQUNkbkMsTUFBZixDQUFzQlUsV0FBdEIsQ0FBa0NKLGVBQWVWLEdBQWpEOzJCQUNlZSxHQUFmLENBQW1CSixPQUFuQixFQUE0QnVCLElBQTVCO1dBRkY7U0FERixNQUtLO2VBQ0U5QixNQUFMLENBQVlTLFFBQVosQ0FBcUIsWUFBVzsyQkFDZlQsTUFBZixDQUFzQlUsV0FBdEIsQ0FBa0NKLGVBQWVWLEdBQWpEOzJCQUNlZSxHQUFmLENBQW1CSixPQUFuQixFQUE0QkEsUUFBUUssSUFBUixDQUFhcUIsTUFBYixDQUE1QjtXQUZGOztPQVZFLE1BZ0JEO2FBQ0VqQyxNQUFMLENBQVlTLFFBQVosQ0FBcUIsWUFBVzt5QkFDZlQsTUFBZixDQUFzQlUsV0FBdEIsQ0FBa0NKLGVBQWVWLEdBQWpEO3lCQUNlZSxHQUFmLENBQW1CSixPQUFuQixFQUE0QkEsUUFBUUssSUFBUixDQUFhM0IsS0FBYixDQUE1QjtTQUZGOzs7Ozs7QUNuSVIsTUFBTW1ELFlBQU4sQ0FBbUI7Y0FDTHhDLEdBQVosRUFBZ0I7U0FDVEEsR0FBTCxHQUFXQSxHQUFYO1NBQ0t5QyxLQUFMLEdBQWEsRUFBYjs7O1VBR0s7V0FDRSxLQUFLQSxLQUFMLENBQVcxRCxNQUFYLEtBQXNCLENBQTdCOzs7TUFHRTJELElBQUosRUFBUztTQUNGRCxLQUFMLENBQVczRCxJQUFYLENBQWdCNEQsSUFBaEI7OztTQUdJO1dBQ0csS0FBS0QsS0FBTCxDQUFXRSxLQUFYLEVBQVA7Ozs7QUFJSixNQUFNQyxTQUFOLENBQWdCO2NBQ0FDLFdBQVcsQ0FBdkIsRUFBMEJDLHlCQUF5QixDQUFuRCxFQUFxRDtTQUM1Q0MsU0FBTCxHQUFpQixLQUFqQjtTQUNLQyxXQUFMLEdBQW1CLFVBQVVDLFFBQVYsRUFBb0I7aUJBQWFBLFFBQVgsRUFBcUJKLFFBQXJCO0tBQXpDOzs7O1NBSUtDLHNCQUFMLEdBQThCQSxzQkFBOUI7U0FDS0ksTUFBTCxHQUFjLElBQUlDLEdBQUosRUFBZDtTQUNLcEMsR0FBTDs7O2FBR0tmLEdBQVgsRUFBZ0IwQyxJQUFoQixFQUFxQjtRQUNoQixDQUFDLEtBQUtRLE1BQUwsQ0FBWUUsR0FBWixDQUFnQnBELEdBQWhCLENBQUosRUFBeUI7V0FDbEJrRCxNQUFMLENBQVlHLEdBQVosQ0FBZ0JyRCxHQUFoQixFQUFxQixJQUFJd0MsWUFBSixDQUFpQnhDLEdBQWpCLENBQXJCOzs7U0FHR2tELE1BQUwsQ0FBWXJCLEdBQVosQ0FBZ0I3QixHQUFoQixFQUFxQnNELEdBQXJCLENBQXlCWixJQUF6Qjs7O1lBR1ExQyxHQUFWLEVBQWM7U0FDUCtDLFNBQUwsR0FBaUIsSUFBakI7O1NBRUtHLE1BQUwsQ0FBWUssTUFBWixDQUFtQnZELEdBQW5COztTQUVLK0MsU0FBTCxHQUFpQixLQUFqQjs7O1FBR0c7UUFDQyxLQUFLQSxTQUFULEVBQW9CO1dBQ2JDLFdBQUwsQ0FBaUIsTUFBTTthQUFPakMsR0FBTDtPQUF6QjtLQURGLE1BRU87V0FDRCxJQUFJLENBQUNmLEdBQUQsRUFBTXdELEtBQU4sQ0FBUixJQUF3QixLQUFLTixNQUE3QixFQUFvQztZQUM5Qk8sYUFBYSxDQUFqQjtlQUNNRCxTQUFTLENBQUNBLE1BQU1FLEtBQU4sRUFBVixJQUEyQkQsYUFBYSxLQUFLWCxzQkFBbkQsRUFBMEU7Y0FDcEVKLE9BQU9jLE1BQU14QyxJQUFOLEVBQVg7ZUFDSytCLFNBQUwsR0FBaUIsSUFBakI7O2NBRUlWLE1BQUo7O2NBRUc7cUJBQ1FLLE1BQVQ7V0FERixDQUVDLE9BQU10QixDQUFOLEVBQVE7b0JBQ0NDLEtBQVIsQ0FBY0QsQ0FBZDtxQkFDU0EsQ0FBVDs7O2VBR0cyQixTQUFMLEdBQWlCLEtBQWpCOztjQUVJVixrQkFBa0JzQixLQUF0QixFQUE2QjtrQkFDckJ0QixNQUFOOzs7Ozs7O1dBT0RXLFdBQUwsQ0FBaUIsTUFBTTthQUFPakMsR0FBTDtPQUF6Qjs7OztpQkFJV2YsR0FBZixFQUFvQjBDLElBQXBCLEVBQTBCa0IsVUFBVSxDQUFwQyxFQUF1QztRQUNsQ0EsWUFBWSxDQUFmLEVBQWlCO1dBQ1ZaLFdBQUwsQ0FBaUIsTUFBTTthQUNoQmEsVUFBTCxDQUFnQjdELEdBQWhCLEVBQXFCMEMsSUFBckI7T0FERjtLQURGLE1BSUs7aUJBQ1EsTUFBTTthQUNWbUIsVUFBTCxDQUFnQjdELEdBQWhCLEVBQXFCMEMsSUFBckI7T0FERixFQUVHa0IsT0FGSDs7OztXQU1LNUQsR0FBVCxFQUFjMEMsSUFBZCxFQUFtQjtTQUNab0IsY0FBTCxDQUFvQjlELEdBQXBCLEVBQXlCLE1BQU07O0tBQS9COzs7aUJBR2FBLEdBQWYsRUFBb0I0RCxPQUFwQixFQUE2QmxCLElBQTdCLEVBQWtDO1NBQzNCb0IsY0FBTCxDQUFvQjlELEdBQXBCLEVBQXlCLE1BQU07O0tBQS9CLEVBQTRDNEQsT0FBNUM7Ozs7QUNuR0o7QUFDQSxBQVNBLE1BQU1HLGFBQU4sQ0FBb0I7O2dCQUVMO1NBQ05DLElBQUwsR0FBWSxJQUFJYixHQUFKLEVBQVo7U0FDS2MsU0FBTCxHQUFpQixJQUFJZCxHQUFKLEVBQWpCO1NBQ0tlLEtBQUwsR0FBYSxJQUFJZixHQUFKLEVBQWI7U0FDS2dCLEtBQUwsR0FBYSxJQUFJaEIsR0FBSixFQUFiO1NBQ0sxQyxRQUFMLEdBQWdCLElBQUkwQyxHQUFKLEVBQWhCOztVQUVNTixXQUFXLENBQWpCLENBUFc7U0FRTnVCLGVBQUwsR0FBdUIsSUFBdkI7U0FDS0MsU0FBTCxHQUFpQixJQUFJekIsU0FBSixDQUFjQyxRQUFkLENBQWpCO1NBQ0t5QixTQUFMLEdBQWlCLElBQUluQixHQUFKLEVBQWpCOztRQUVJb0IsdUJBQXVCLElBQTNCO1NBQ0tDLGdCQUFMLEdBQXdCLEtBQUtDLEtBQUwsQ0FBVyxhQUFXO1lBQ3RDRixxQkFBcUJHLEtBQXJCLENBQTJCeEYsT0FBT0MsR0FBUCxDQUFXLFVBQVgsQ0FBM0IsQ0FBTjtLQURzQixDQUF4QjtTQUdLMkIsV0FBTCxDQUFpQixLQUFLMEQsZ0JBQXRCOzs7VUFHT3pELEdBQVQsQ0FBYVksR0FBYixFQUFrQnpCLElBQWxCLEVBQXdCeUUsVUFBVSxJQUFsQyxFQUF1QztRQUNsQ2hELElBQUlLLFdBQUosQ0FBZ0JDLElBQWhCLEtBQXlCLG1CQUE1QixFQUFnRDthQUN2QyxPQUFPTixJQUFJUixLQUFKLENBQVV3RCxPQUFWLEVBQW1CekUsSUFBbkIsQ0FBZDtLQURGLE1BRUs7YUFDSSxNQUFNeUIsSUFBSVIsS0FBSixDQUFVd0QsT0FBVixFQUFtQnpFLElBQW5CLENBQWI7Ozs7UUFJRSxHQUFHQSxJQUFULEVBQWM7UUFDVEEsS0FBS25CLE1BQUwsS0FBZ0IsQ0FBbkIsRUFBcUI7VUFDZjRDLE1BQU16QixLQUFLLENBQUwsQ0FBVjthQUNPLEtBQUswRSxRQUFMLENBQWNqRCxHQUFkLEVBQW1CLEVBQW5CLEVBQXVCLEtBQXZCLEVBQThCM0IsR0FBckM7S0FGRixNQUlLO1VBQ0M2RSxNQUFNM0UsS0FBSyxDQUFMLENBQVY7VUFDSXlCLE1BQU16QixLQUFLLENBQUwsQ0FBVjtVQUNJNEUsV0FBVzVFLEtBQUssQ0FBTCxDQUFmOzthQUVPLEtBQUswRSxRQUFMLENBQWNDLElBQUlsRCxHQUFKLENBQWQsRUFBd0JtRCxRQUF4QixFQUFrQyxLQUFsQyxFQUF5QyxLQUF6QyxFQUFnRDlFLEdBQXZEOzs7O2FBSU8sR0FBR0UsSUFBZCxFQUFtQjtRQUNkQSxLQUFLbkIsTUFBTCxLQUFnQixDQUFuQixFQUFxQjtVQUNmNEMsTUFBTXpCLEtBQUssQ0FBTCxDQUFWO2FBQ08sS0FBSzBFLFFBQUwsQ0FBY2pELEdBQWQsRUFBbUIsRUFBbkIsRUFBdUIsSUFBdkIsRUFBNkIsS0FBN0IsRUFBb0MzQixHQUEzQztLQUZGLE1BSUs7VUFDQzZFLE1BQU0zRSxLQUFLLENBQUwsQ0FBVjtVQUNJeUIsTUFBTXpCLEtBQUssQ0FBTCxDQUFWO1VBQ0k0RSxXQUFXNUUsS0FBSyxDQUFMLENBQWY7O2FBRU8sS0FBSzBFLFFBQUwsQ0FBY0MsSUFBSWxELEdBQUosQ0FBZCxFQUF3Qm1ELFFBQXhCLEVBQWtDLElBQWxDLEVBQXdDLEtBQXhDLEVBQStDOUUsR0FBdEQ7Ozs7T0FJQ0EsR0FBTCxFQUFTO1NBQ0ZtRSxLQUFMLENBQVd0QyxHQUFYLENBQWUsS0FBSzdCLEdBQUwsRUFBZixFQUEyQnNELEdBQTNCLENBQStCdEQsR0FBL0I7U0FDS21FLEtBQUwsQ0FBV3RDLEdBQVgsQ0FBZTdCLEdBQWYsRUFBb0JzRCxHQUFwQixDQUF3QixLQUFLdEQsR0FBTCxFQUF4Qjs7O1NBR0tBLEdBQVAsRUFBVztTQUNKbUUsS0FBTCxDQUFXdEMsR0FBWCxDQUFlLEtBQUs3QixHQUFMLEVBQWYsRUFBMkJ1RCxNQUEzQixDQUFrQ3ZELEdBQWxDO1NBQ0ttRSxLQUFMLENBQVd0QyxHQUFYLENBQWU3QixHQUFmLEVBQW9CdUQsTUFBcEIsQ0FBMkIsS0FBS3ZELEdBQUwsRUFBM0I7OztnQkFHWSxHQUFHRSxJQUFqQixFQUFzQjtRQUNqQkEsS0FBS25CLE1BQUwsS0FBZ0IsQ0FBbkIsRUFBcUI7VUFDZjRDLE1BQU16QixLQUFLLENBQUwsQ0FBVjtVQUNJNkUsVUFBVSxLQUFLSCxRQUFMLENBQWNqRCxHQUFkLEVBQW1CLEVBQW5CLEVBQXVCLEtBQXZCLEVBQThCLElBQTlCLENBQWQ7YUFDTyxDQUFDb0QsUUFBUS9FLEdBQVQsRUFBYytFLFFBQVF0RSxRQUFSLENBQWlCLENBQWpCLENBQWQsQ0FBUDtLQUhGLE1BS0s7VUFDQ29FLE1BQU0zRSxLQUFLLENBQUwsQ0FBVjtVQUNJeUIsTUFBTXpCLEtBQUssQ0FBTCxDQUFWO1VBQ0k0RSxXQUFXNUUsS0FBSyxDQUFMLENBQWY7VUFDSTZFLFVBQVUsS0FBS0gsUUFBTCxDQUFjQyxJQUFJbEQsR0FBSixDQUFkLEVBQXdCbUQsUUFBeEIsRUFBa0MsS0FBbEMsRUFBeUMsSUFBekMsQ0FBZDs7YUFFTyxDQUFDQyxRQUFRL0UsR0FBVCxFQUFjK0UsUUFBUXRFLFFBQVIsQ0FBaUIsQ0FBakIsQ0FBZCxDQUFQOzs7O1VBSUlULEdBQVIsRUFBWTtVQUNKZ0YsV0FBVyxLQUFLQyxLQUFMLENBQVdqRixHQUFYLENBQWpCO1VBQ01rRixNQUFNLEtBQUtDLFFBQUwsRUFBWjs7UUFFR0gsUUFBSCxFQUFZOztXQUVMdkUsUUFBTCxDQUFjNEMsR0FBZCxDQUFrQjZCLEdBQWxCLEVBQXVCLEVBQUMsV0FBVyxLQUFLZCxlQUFMLENBQXFCcEUsR0FBakMsRUFBc0MsV0FBV2dGLFFBQWpELEVBQXZCO1dBQ0toQixJQUFMLENBQVVuQyxHQUFWLENBQWNtRCxRQUFkLEVBQXdCdkUsUUFBeEIsQ0FBaUMzQixJQUFqQyxDQUFzQ29HLEdBQXRDO2FBQ09BLEdBQVA7S0FKRixNQUtLO1dBQ0VFLElBQUwsQ0FBVSxLQUFLaEIsZUFBTCxDQUFxQnBFLEdBQS9CLEVBQW9DLElBQUlxRixZQUFZQyxLQUFoQixDQUFzQixNQUF0QixFQUE4QkosR0FBOUIsRUFBbUNsRixHQUFuQyxFQUF3Q2dGLFFBQXhDLEVBQWtEOUYsT0FBT0MsR0FBUCxDQUFXLFFBQVgsQ0FBbEQsQ0FBcEM7YUFDTytGLEdBQVA7Ozs7WUFJTUEsR0FBVixFQUFjO1FBQ1QsS0FBS0ssT0FBTCxDQUFhbkMsR0FBYixDQUFpQjhCLEdBQWpCLENBQUgsRUFBeUI7V0FDbEJLLE9BQUwsQ0FBYWhDLE1BQWIsQ0FBb0IyQixHQUFwQjthQUNPLElBQVA7OztXQUdLLEtBQVA7OztjQUdVTSxFQUFaLEVBQWU7UUFDVHhGLE1BQU0sS0FBS2lGLEtBQUwsQ0FBV08sRUFBWCxDQUFWO1FBQ0d4RixRQUFRLElBQVgsRUFBZ0I7V0FDVG9FLGVBQUwsR0FBdUIsS0FBS0osSUFBTCxDQUFVbkMsR0FBVixDQUFjN0IsR0FBZCxDQUF2QjtXQUNLb0UsZUFBTCxDQUFxQi9ELE1BQXJCLEdBQThCYixPQUFPaUcsT0FBckM7Ozs7V0FJSzlELEdBQVQsRUFBY3pCLElBQWQsRUFBb0J3RixNQUFwQixFQUE0QkMsU0FBNUIsRUFBc0M7UUFDaENDLFNBQVMsSUFBSVAsWUFBWVEsR0FBaEIsRUFBYjtRQUNJMUYsVUFBVSxJQUFJeEIsT0FBSixFQUFkO1FBQ0ltSCxVQUFVLElBQUkvRixPQUFKLENBQVk2RixNQUFaLEVBQW9CakUsR0FBcEIsRUFBeUJ6QixJQUF6QixFQUErQkMsT0FBL0IsRUFBd0MsSUFBeEMsQ0FBZDs7U0FFSzZELElBQUwsQ0FBVVgsR0FBVixDQUFjdUMsTUFBZCxFQUFzQkUsT0FBdEI7U0FDSzdCLFNBQUwsQ0FBZVosR0FBZixDQUFtQnVDLE1BQW5CLEVBQTJCekYsT0FBM0I7U0FDS2dFLEtBQUwsQ0FBV2QsR0FBWCxDQUFldUMsTUFBZixFQUF1QixJQUFJRyxHQUFKLEVBQXZCOztRQUVHTCxNQUFILEVBQVU7V0FDSE0sSUFBTCxDQUFVSixNQUFWOzs7UUFHQ0QsU0FBSCxFQUFhO1dBQ05KLE9BQUwsQ0FBYUssTUFBYjs7O1lBR01LLEtBQVI7V0FDT0gsT0FBUDs7O2NBR1U5RixHQUFaLEVBQWlCa0csVUFBakIsRUFBNEI7U0FDckJsQyxJQUFMLENBQVVULE1BQVYsQ0FBaUJ2RCxHQUFqQjtTQUNLbUcsVUFBTCxDQUFnQm5HLEdBQWhCO1NBQ0txRSxTQUFMLENBQWUrQixTQUFmLENBQXlCcEcsR0FBekI7O1FBRUcsS0FBS21FLEtBQUwsQ0FBV2YsR0FBWCxDQUFlcEQsR0FBZixDQUFILEVBQXVCO1dBQ2hCLElBQUlxRyxPQUFULElBQW9CLEtBQUtsQyxLQUFMLENBQVd0QyxHQUFYLENBQWU3QixHQUFmLENBQXBCLEVBQXlDO2FBQ2xDc0IsSUFBTCxDQUFVK0UsT0FBVixFQUFtQkgsVUFBbkI7YUFDSy9CLEtBQUwsQ0FBV3RDLEdBQVgsQ0FBZXdFLE9BQWYsRUFBd0I5QyxNQUF4QixDQUErQnZELEdBQS9COzs7V0FHR21FLEtBQUwsQ0FBV1osTUFBWCxDQUFrQnZELEdBQWxCOzs7O1dBSUtpQyxJQUFULEVBQWVqQyxHQUFmLEVBQW1CO1FBQ2QsQ0FBQyxLQUFLa0UsS0FBTCxDQUFXZCxHQUFYLENBQWVuQixJQUFmLENBQUosRUFBeUI7V0FDbEJpQyxLQUFMLENBQVdiLEdBQVgsQ0FBZXBCLElBQWYsRUFBcUJqQyxHQUFyQjtLQURGLE1BRUs7WUFDRyxJQUFJMkQsS0FBSixDQUFVLCtDQUFWLENBQU47Ozs7VUFJSTFCLElBQVIsRUFBYTtXQUNKLEtBQUtpQyxLQUFMLENBQVdkLEdBQVgsQ0FBZW5CLElBQWYsSUFBdUIsS0FBS2lDLEtBQUwsQ0FBV3JDLEdBQVgsQ0FBZUksSUFBZixDQUF2QixHQUE4QyxJQUFyRDs7O2VBR1U7V0FDSCxLQUFLaUMsS0FBTCxDQUFXb0MsSUFBWCxFQUFQOzs7YUFHU3RHLEdBQVgsRUFBZTtTQUNULElBQUlpQyxJQUFSLElBQWdCLEtBQUtpQyxLQUFMLENBQVdvQyxJQUFYLEVBQWhCLEVBQWtDO1VBQzdCLEtBQUtwQyxLQUFMLENBQVdkLEdBQVgsQ0FBZW5CLElBQWYsS0FBd0IsS0FBS2lDLEtBQUwsQ0FBV3JDLEdBQVgsQ0FBZUksSUFBZixNQUF5QmpDLEdBQXBELEVBQXdEO2FBQ2pEa0UsS0FBTCxDQUFXWCxNQUFYLENBQWtCdEIsSUFBbEI7Ozs7O1FBS0Q7V0FDSSxLQUFLbUMsZUFBTCxDQUFxQnBFLEdBQTVCOzs7UUFHSXdGLEVBQU4sRUFBUztRQUNIQSxjQUFjSCxZQUFZUSxHQUE5QixFQUFtQzthQUN6QixLQUFLN0IsSUFBTCxDQUFVWixHQUFWLENBQWNvQyxFQUFkLElBQW9CQSxFQUFwQixHQUF5QixJQUFoQztLQURILE1BRU8sSUFBSUEsY0FBY3pGLE9BQWxCLEVBQTJCO2FBQ3hCeUYsR0FBR3hGLEdBQVY7S0FESSxNQUVBO1VBQ0FBLE1BQU0sS0FBS3VHLE9BQUwsQ0FBYWYsRUFBYixDQUFWO1VBQ0l4RixRQUFRLElBQVosRUFDRyxNQUFNLGtDQUFrQ3dGLEVBQWxDLEdBQXVDLElBQXZDLEdBQThDLE9BQU9BLEVBQXJELEdBQTJELEdBQWpFO2FBQ0l4RixHQUFQOzs7O09BSUF3RixFQUFMLEVBQVNnQixHQUFULEVBQWM7VUFDTnhHLE1BQU0sS0FBS2lGLEtBQUwsQ0FBV08sRUFBWCxDQUFaOztRQUVHeEYsR0FBSCxFQUFPO1dBQ0FpRSxTQUFMLENBQWVwQyxHQUFmLENBQW1CN0IsR0FBbkIsRUFBd0J5RyxPQUF4QixDQUFnQ0QsR0FBaEM7O1VBRUcsS0FBS2xDLFNBQUwsQ0FBZWxCLEdBQWYsQ0FBbUJwRCxHQUFuQixDQUFILEVBQTJCO1lBQ3JCMkIsTUFBTSxLQUFLMkMsU0FBTCxDQUFlekMsR0FBZixDQUFtQjdCLEdBQW5CLENBQVY7YUFDS3NFLFNBQUwsQ0FBZWYsTUFBZixDQUFzQnZELEdBQXRCO2FBQ0thLFFBQUwsQ0FBY2MsR0FBZDs7OztXQUlHNkUsR0FBUDs7O1VBR003RSxHQUFSLEVBQWErRSxVQUFVLENBQXZCLEVBQTBCQyxZQUFZLE1BQU0sSUFBNUMsRUFBbUQ7UUFDN0NDLGNBQWMsSUFBbEI7O1FBRUdGLFlBQVksQ0FBWixJQUFpQkEsWUFBWUcsUUFBaEMsRUFBeUM7b0JBQ3pCLElBQWQ7S0FERixNQUVLO29CQUNXaEgsS0FBS0MsR0FBTCxLQUFhNEcsT0FBM0I7OztXQUdLLENBQ0xsSCxPQUFPRyxPQURGLEVBRUxnQyxHQUZLLEVBR0xpRixXQUhLLEVBSUxELFNBSkssQ0FBUDs7O1FBUUlHLFFBQU4sRUFBZTtXQUNOLENBQUN0SCxPQUFPQyxLQUFSLEVBQWVxSCxRQUFmLENBQVA7OztVQUdNbkYsR0FBUixFQUFZO1NBQ0x5QyxlQUFMLENBQXFCL0QsTUFBckIsR0FBOEJiLE9BQU91SCxTQUFyQztTQUNLekMsU0FBTCxDQUFlakIsR0FBZixDQUFtQixLQUFLZSxlQUFMLENBQXFCcEUsR0FBeEMsRUFBNkMyQixHQUE3Qzs7O1FBR0lBLEdBQU4sRUFBV3FGLElBQVgsRUFBZ0I7U0FDVDVDLGVBQUwsQ0FBcUIvRCxNQUFyQixHQUE4QmIsT0FBT3lILFFBQXJDOztRQUVHQyxPQUFPQyxTQUFQLENBQWlCSCxJQUFqQixDQUFILEVBQTBCO1dBQ25CM0MsU0FBTCxDQUFlK0MsY0FBZixDQUE4QixLQUFLaEQsZUFBTCxDQUFxQnBFLEdBQW5ELEVBQXdEZ0gsSUFBeEQsRUFBOERyRixHQUE5RDs7OztXQUlLQSxHQUFULEVBQWMzQixHQUFkLEVBQWtCO1VBQ1ZxSCxVQUFVckgsT0FBTyxJQUFQLEdBQWNBLEdBQWQsR0FBb0IsS0FBS29FLGVBQUwsQ0FBcUJwRSxHQUF6RDtTQUNLcUUsU0FBTCxDQUFleEQsUUFBZixDQUF3QndHLE9BQXhCLEVBQWlDMUYsR0FBakM7OztPQUdHMkYsR0FBTCxFQUFVQyxHQUFWLEVBQWM7UUFDUnZILE1BQU0sSUFBVjtRQUNJeUIsU0FBUyxJQUFiO1FBQ0lzRCxVQUFVLElBQWQ7O1FBRUd3QyxHQUFILEVBQU87WUFDQ0QsR0FBTjtlQUNTQyxHQUFUO2dCQUNVLEtBQUt2RCxJQUFMLENBQVVuQyxHQUFWLENBQWMsS0FBS29ELEtBQUwsQ0FBV2pGLEdBQVgsQ0FBZCxDQUFWOztVQUVJK0UsV0FBV0EsUUFBUXlDLGlCQUFSLEVBQVosSUFBNEMvRixXQUFXakMsT0FBT2lJLElBQTlELElBQXNFaEcsV0FBV2pDLE9BQU8wQixNQUEzRixFQUFrRzthQUMzRitDLFNBQUwsQ0FBZXBDLEdBQWYsQ0FBbUJrRCxRQUFRL0UsR0FBM0IsRUFBZ0N5RyxPQUFoQyxDQUF3QyxJQUFJcEIsWUFBWUMsS0FBaEIsQ0FBc0I5RixPQUFPa0ksSUFBN0IsRUFBbUMsS0FBSzFILEdBQUwsRUFBbkMsRUFBK0N5QixNQUEvQyxDQUF4QztPQURGLE1BRU07Z0JBQ0lrRyxNQUFSLENBQWVsRyxNQUFmOztLQVJKLE1BV0s7WUFDRyxLQUFLMkMsZUFBTCxDQUFxQnBFLEdBQTNCO2VBQ1NzSCxHQUFUO2dCQUNVLEtBQUtsRCxlQUFmOztjQUVRdUQsTUFBUixDQUFlbEcsTUFBZjs7O1NBR0UsSUFBSXlELEdBQVIsSUFBZUgsUUFBUXRFLFFBQXZCLEVBQWdDO1VBQzFCbUgsT0FBTyxLQUFLbkgsUUFBTCxDQUFjb0IsR0FBZCxDQUFrQnFELEdBQWxCLENBQVg7V0FDS0UsSUFBTCxDQUFVd0MsS0FBSyxTQUFMLENBQVYsRUFBMkIsSUFBSXZDLFlBQVlDLEtBQWhCLENBQXNCLE1BQXRCLEVBQThCSixHQUE5QixFQUFtQzBDLEtBQUssU0FBTCxDQUFuQyxFQUFvREEsS0FBSyxTQUFMLENBQXBELEVBQXFFbkcsTUFBckUsQ0FBM0I7Ozs7UUFJRUEsTUFBTixFQUFhO1NBQ04yQyxlQUFMLENBQXFCdUQsTUFBckIsQ0FBNEJsRyxNQUE1Qjs7O2VBR1csR0FBR3ZCLElBQWhCLEVBQXFCO1FBQ2hCQSxLQUFLbkIsTUFBTCxJQUFlLENBQWxCLEVBQW9CO1lBQ1p3QyxPQUFPckIsS0FBSyxDQUFMLENBQWI7WUFDTWIsUUFBUWEsS0FBSyxDQUFMLENBQWQ7YUFDTyxLQUFLa0UsZUFBTCxDQUFxQnlELFlBQXJCLENBQWtDdEcsSUFBbEMsRUFBd0NsQyxLQUF4QyxDQUFQO0tBSEYsTUFJSztZQUNHVyxNQUFNLEtBQUtpRixLQUFMLENBQVcvRSxLQUFLLENBQUwsQ0FBWCxDQUFaO1lBQ01xQixPQUFPckIsS0FBSyxDQUFMLENBQWI7WUFDTWIsUUFBUWEsS0FBSyxDQUFMLENBQWQ7YUFDTyxLQUFLOEQsSUFBTCxDQUFVbkMsR0FBVixDQUFjN0IsR0FBZCxFQUFtQjZILFlBQW5CLENBQWdDdEcsSUFBaEMsRUFBc0NsQyxLQUF0QyxDQUFQOzs7O01BSUF5SSxHQUFKLEVBQVN6SSxLQUFULEVBQWU7U0FDUitFLGVBQUwsQ0FBcUI3RCxJQUFyQixDQUEwQnVILEdBQTFCLElBQWlDekksS0FBakM7OztxQkFHZ0I7V0FDVCxLQUFLK0UsZUFBTCxDQUFxQjdELElBQTVCOzs7TUFHRXVILEdBQUosRUFBU0MsZ0JBQWdCLElBQXpCLEVBQThCO1FBQ3pCRCxPQUFPLEtBQUsxRCxlQUFMLENBQXFCN0QsSUFBL0IsRUFBb0M7YUFDM0IsS0FBSzZELGVBQUwsQ0FBcUI3RCxJQUFyQixDQUEwQnVILEdBQTFCLENBQVA7S0FERixNQUVLO2FBQ0lDLGFBQVA7Ozs7V0FJSzFJLEtBQVQsRUFBZTtRQUNWQSxLQUFILEVBQVM7VUFDSGlILE9BQU8sRUFBWDs7V0FFSSxJQUFJd0IsR0FBUixJQUFlRSxPQUFPMUIsSUFBUCxDQUFZLEtBQUtsQyxlQUFMLENBQXFCN0QsSUFBakMsQ0FBZixFQUFzRDtZQUNqRCxLQUFLNkQsZUFBTCxDQUFxQjdELElBQXJCLENBQTBCdUgsR0FBMUIsTUFBbUN6SSxLQUF0QyxFQUE0QztlQUNyQ1AsSUFBTCxDQUFVZ0osR0FBVjs7OzthQUlHeEIsSUFBUDs7O1dBR0swQixPQUFPMUIsSUFBUCxDQUFZLEtBQUtsQyxlQUFMLENBQXFCN0QsSUFBakMsQ0FBUDs7O1FBR0l1SCxHQUFOLEVBQVU7UUFDTEEsT0FBTyxJQUFWLEVBQWU7YUFDTixLQUFLMUQsZUFBTCxDQUFxQjdELElBQXJCLENBQTBCdUgsR0FBMUIsQ0FBUDtLQURGLE1BRUs7V0FDRTFELGVBQUwsQ0FBcUI3RCxJQUFyQixHQUE0QixFQUE1Qjs7OztXQUlLUCxHQUFULEVBQWE7VUFDTGdGLFdBQVcsS0FBS0MsS0FBTCxDQUFXakYsR0FBWCxDQUFqQjtXQUNPZ0YsWUFBWSxJQUFuQjs7O1NBR0k7V0FDRzFGLE1BQU0ySSxJQUFOLENBQVcsS0FBS2pFLElBQUwsQ0FBVXNDLElBQVYsRUFBWCxDQUFQOzs7YUFHUTtXQUNELElBQUlqQixZQUFZNkMsU0FBaEIsRUFBUDs7OztBQ2hXSixZQUFlOztDQUFmOzs7OyJ9
