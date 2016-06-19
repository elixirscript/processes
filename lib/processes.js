"use strict";

var _slicedToArray = (function () { function sliceIterator(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"]) _i["return"](); } finally { if (_d) throw _e; } } return _arr; } return function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { return sliceIterator(arr, i); } else { throw new TypeError("Invalid attempt to destructure non-iterable instance"); } }; })();

var _createClass = (function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; })();

Object.defineProperty(exports, "__esModule", {
  value: true
});

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

let Mailbox = (function () {
  function Mailbox() {
    _classCallCheck(this, Mailbox);

    this.messages = [];
  }

  _createClass(Mailbox, [{
    key: "deliver",
    value: function deliver(message) {
      this.messages.push(message);
      return message;
    }
  }, {
    key: "get",
    value: function get() {
      return this.messages;
    }
  }, {
    key: "isEmpty",
    value: function isEmpty() {
      return this.messages.length === 0;
    }
  }, {
    key: "removeAt",
    value: function removeAt(index) {
      this.messages.splice(index, 1);
    }
  }]);

  return Mailbox;
})();

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

let Process = (function () {
  function Process(pid, func, args, mailbox, system) {
    _classCallCheck(this, Process);

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

  _createClass(Process, [{
    key: "start",
    value: function start() {
      const function_scope = this;
      let machine = this.main();

      this.system.schedule(function () {
        function_scope.system.set_current(function_scope.pid);
        function_scope.run(machine, machine.next());
      }, this.pid);
    }
  }, {
    key: "main",
    value: function* main() {
      let retval = States.NORMAL;

      try {
        yield* this.func.apply(null, this.args);
      } catch (e) {
        console.error(e);
        retval = e;
      }

      this.system.exit(retval);
    }
  }, {
    key: "process_flag",
    value: function process_flag(flag, value) {
      const old_value = this.flags[flag];
      this.flags[flag] = value;
      return old_value;
    }
  }, {
    key: "is_trapping_exits",
    value: function is_trapping_exits() {
      return this.flags[Symbol.for("trap_exit")] && this.flags[Symbol.for("trap_exit")] == true;
    }
  }, {
    key: "signal",
    value: function signal(reason) {
      if (reason !== States.NORMAL) {
        console.error(reason);
      }

      this.system.remove_proc(this.pid, reason);
    }
  }, {
    key: "receive",
    value: function receive(fun) {
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
  }, {
    key: "run",
    value: function run(machine, step) {
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
  }]);

  return Process;
})();

let ProcessQueue = (function () {
  function ProcessQueue(pid) {
    _classCallCheck(this, ProcessQueue);

    this.pid = pid;
    this.tasks = [];
  }

  _createClass(ProcessQueue, [{
    key: "empty",
    value: function empty() {
      return this.tasks.length === 0;
    }
  }, {
    key: "add",
    value: function add(task) {
      this.tasks.push(task);
    }
  }, {
    key: "next",
    value: function next() {
      return this.tasks.shift();
    }
  }]);

  return ProcessQueue;
})();

let Scheduler = (function () {
  function Scheduler() {
    let throttle = arguments.length <= 0 || arguments[0] === undefined ? 0 : arguments[0];
    let reductions_per_process = arguments.length <= 1 || arguments[1] === undefined ? 8 : arguments[1];

    _classCallCheck(this, Scheduler);

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

  _createClass(Scheduler, [{
    key: "addToQueue",
    value: function addToQueue(pid, task) {
      if (!this.queues.has(pid)) {
        this.queues.set(pid, new ProcessQueue(pid));
      }

      this.queues.get(pid).add(task);
    }
  }, {
    key: "removePid",
    value: function removePid(pid) {
      this.isRunning = true;

      this.queues.delete(pid);

      this.isRunning = false;
    }
  }, {
    key: "run",
    value: function run() {
      if (this.isRunning) {
        this.invokeLater(() => {
          this.run();
        });
      } else {
        for (let _ref of this.queues) {
          var _ref2 = _slicedToArray(_ref, 2);

          let pid = _ref2[0];
          let queue = _ref2[1];

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
  }, {
    key: "addToScheduler",
    value: function addToScheduler(pid, task) {
      let dueTime = arguments.length <= 2 || arguments[2] === undefined ? 0 : arguments[2];

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
  }, {
    key: "schedule",
    value: function schedule(pid, task) {
      this.addToScheduler(pid, () => {
        task();
      });
    }
  }, {
    key: "scheduleFuture",
    value: function scheduleFuture(pid, dueTime, task) {
      this.addToScheduler(pid, () => {
        task();
      }, dueTime);
    }
  }]);

  return Scheduler;
})();

let process_counter = -1;

let PID = (function () {
  function PID() {
    _classCallCheck(this, PID);

    process_counter = process_counter + 1;
    this.id = process_counter;
  }

  _createClass(PID, [{
    key: "toString",
    value: function toString() {
      return "PID#<0." + this.id + ".0>";
    }
  }]);

  return PID;
})();

let ref_counter = -1;

let Reference = (function () {
  function Reference() {
    _classCallCheck(this, Reference);

    ref_counter = ref_counter + 1;
    this.id = ref_counter;
    this.ref = Symbol();
  }

  _createClass(Reference, [{
    key: "toString",
    value: function toString() {
      return "Ref#<0.0.0." + this.id + ">";
    }
  }]);

  return Reference;
})();

let ProcessSystem = (function () {
  function ProcessSystem() {
    _classCallCheck(this, ProcessSystem);

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

  _createClass(ProcessSystem, [{
    key: "spawn",
    value: function spawn() {
      for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
        args[_key] = arguments[_key];
      }

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
  }, {
    key: "spawn_link",
    value: function spawn_link() {
      for (var _len2 = arguments.length, args = Array(_len2), _key2 = 0; _key2 < _len2; _key2++) {
        args[_key2] = arguments[_key2];
      }

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
  }, {
    key: "link",
    value: function link(pid) {
      this.links.get(this.pid()).add(pid);
      this.links.get(pid).add(this.pid());
    }
  }, {
    key: "unlink",
    value: function unlink(pid) {
      this.links.get(this.pid()).delete(pid);
      this.links.get(pid).delete(this.pid());
    }
  }, {
    key: "spawn_monitor",
    value: function spawn_monitor() {
      for (var _len3 = arguments.length, args = Array(_len3), _key3 = 0; _key3 < _len3; _key3++) {
        args[_key3] = arguments[_key3];
      }

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
  }, {
    key: "monitor",
    value: function monitor(pid) {
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
  }, {
    key: "demonitor",
    value: function demonitor(ref) {
      if (this.monitor.has(ref)) {
        this.monitor.delete(ref);
        return true;
      }

      return false;
    }
  }, {
    key: "set_current",
    value: function set_current(id) {
      let pid = this.pidof(id);
      if (pid !== null) {
        this.current_process = this.pids.get(pid);
        this.current_process.status = States.RUNNING;
      }
    }
  }, {
    key: "add_proc",
    value: function add_proc(fun, args, linked, monitored) {
      let newpid = new PID();
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
  }, {
    key: "remove_proc",
    value: function remove_proc(pid, exitreason) {
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
  }, {
    key: "register",
    value: function register(name, pid) {
      if (!this.names.has(name)) {
        this.names.set(name, pid);
      } else {
        throw new Error("Name is already registered to another process");
      }
    }
  }, {
    key: "whereis",
    value: function whereis(name) {
      return this.names.has(name) ? this.names.get(name) : null;
    }
  }, {
    key: "registered",
    value: function registered() {
      return this.names.keys();
    }
  }, {
    key: "unregister",
    value: function unregister(pid) {
      for (let name of this.names.keys()) {
        if (this.names.has(name) && this.names.get(name) === pid) {
          this.names.delete(name);
        }
      }
    }
  }, {
    key: "pid",
    value: function pid() {
      return this.current_process.pid;
    }
  }, {
    key: "pidof",
    value: function pidof(id) {
      if (id instanceof PID) {
        return this.pids.has(id) ? id : null;
      } else if (id instanceof Process) {
        return id.pid;
      } else {
        let pid = this.whereis(id);
        if (pid === null) throw "Process name not registered: " + id + " (" + typeof id + ")";
        return pid;
      }
    }
  }, {
    key: "send",
    value: function send(id, msg) {
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
  }, {
    key: "receive",
    value: function receive(fun) {
      let timeout = arguments.length <= 1 || arguments[1] === undefined ? 0 : arguments[1];
      let timeoutFn = arguments.length <= 2 || arguments[2] === undefined ? () => true : arguments[2];

      let DateTimeout = null;

      if (timeout === 0 || timeout === Infinity) {
        DateTimeout = null;
      } else {
        DateTimeout = Date.now() + timeout;
      }

      return [States.RECEIVE, fun, DateTimeout, timeoutFn];
    }
  }, {
    key: "sleep",
    value: function sleep(duration) {
      return [States.SLEEP, duration];
    }
  }, {
    key: "suspend",
    value: function suspend(fun) {
      this.current_process.status = States.SUSPENDED;
      this.suspended.set(this.current_process.pid, fun);
    }
  }, {
    key: "delay",
    value: function delay(fun, time) {
      this.current_process.status = States.SLEEPING;
      this.scheduler.scheduleFuture(this.current_process.pid, time, fun);
    }
  }, {
    key: "schedule",
    value: function schedule(fun, pid) {
      const the_pid = pid != null ? pid : this.current_process.pid;
      this.scheduler.schedule(the_pid, fun);
    }
  }, {
    key: "exit",
    value: function exit(one, two) {
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
  }, {
    key: "error",
    value: function error(reason) {
      this.current_process.signal(reason);
    }
  }, {
    key: "process_flag",
    value: function process_flag() {
      for (var _len4 = arguments.length, args = Array(_len4), _key4 = 0; _key4 < _len4; _key4++) {
        args[_key4] = arguments[_key4];
      }

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
  }, {
    key: "put",
    value: function put(key, value) {
      this.current_process.dict[key] = value;
    }
  }, {
    key: "get_process_dict",
    value: function get_process_dict() {
      return this.current_process.dict;
    }
  }, {
    key: "get",
    value: function get(key) {
      let default_value = arguments.length <= 1 || arguments[1] === undefined ? null : arguments[1];

      if (key in this.current_process.dict) {
        return this.current_process.dict[key];
      } else {
        return default_value;
      }
    }
  }, {
    key: "get_keys",
    value: function get_keys(value) {
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
  }, {
    key: "erase",
    value: function erase(key) {
      if (key != null) {
        delete this.current_process.dict[key];
      } else {
        this.current_process.dict = {};
      }
    }
  }, {
    key: "is_alive",
    value: function is_alive(pid) {
      const real_pid = this.pidof(pid);
      return real_pid != null;
    }
  }, {
    key: "list",
    value: function list() {
      return this.pids.keys();
    }
  }, {
    key: "make_ref",
    value: function make_ref() {
      return new Reference();
    }
  }], [{
    key: "run",
    value: function* run(fun, args) {
      let context = arguments.length <= 2 || arguments[2] === undefined ? null : arguments[2];

      if (fun.constructor.name === "GeneratorFunction") {
        return yield* fun.apply(context, args);
      } else {
        return yield fun.apply(context, args);
      }
    }
  }]);

  return ProcessSystem;
})();

var index = {
  ProcessSystem: ProcessSystem
};

exports.default = index;
//# sourceMappingURL=data:application/json;base64,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eEMsTUFBTTtBQUNMLGVBQU8sTUFBTSxHQUFHLENBQUMsS0FBSyxDQUFDLE9BQU8sRUFBRSxJQUFJLENBQUMsQ0FBQztPQUN2QztLQUNGOzs7U0E3QkcsYUFBYTs7O0FBZ1ZuQixJQUFJLEtBQUssR0FBRztBQUNWLGVBQWEsRUFBYixhQUFhO0NBQ2QsQ0FBQzs7a0JBRWEsS0FBSyIsImZpbGUiOiJpbmRleC5qcyIsInNvdXJjZXNDb250ZW50IjpbImNsYXNzIE1haWxib3gge1xuICBjb25zdHJ1Y3RvcigpIHtcbiAgICB0aGlzLm1lc3NhZ2VzID0gW107XG4gIH1cblxuICBkZWxpdmVyKG1lc3NhZ2UpIHtcbiAgICB0aGlzLm1lc3NhZ2VzLnB1c2gobWVzc2FnZSk7XG4gICAgcmV0dXJuIG1lc3NhZ2U7XG4gIH1cblxuICBnZXQoKSB7XG4gICAgcmV0dXJuIHRoaXMubWVzc2FnZXM7XG4gIH1cblxuICBpc0VtcHR5KCkge1xuICAgIHJldHVybiB0aGlzLm1lc3NhZ2VzLmxlbmd0aCA9PT0gMDtcbiAgfVxuXG4gIHJlbW92ZUF0KGluZGV4KSB7XG4gICAgdGhpcy5tZXNzYWdlcy5zcGxpY2UoaW5kZXgsIDEpO1xuICB9XG59XG5cbnZhciBTdGF0ZXMgPSB7XG4gIE5PUk1BTDogU3ltYm9sLmZvcihcIm5vcm1hbFwiKSxcbiAgS0lMTDogU3ltYm9sLmZvcihcImtpbGxcIiksXG4gIFNVU1BFTkQ6IFN5bWJvbC5mb3IoXCJzdXNwZW5kXCIpLFxuICBDT05USU5VRTogU3ltYm9sLmZvcihcImNvbnRpbnVlXCIpLFxuICBSRUNFSVZFOiBTeW1ib2wuZm9yKFwicmVjZWl2ZVwiKSxcbiAgU0VORDogU3ltYm9sLmZvcihcInNlbmRcIiksXG4gIFNMRUVQSU5HOiBTeW1ib2wuZm9yKFwic2xlZXBpbmdcIiksXG4gIFJVTk5JTkc6IFN5bWJvbC5mb3IoXCJydW5uaW5nXCIpLFxuICBTVVNQRU5ERUQ6IFN5bWJvbC5mb3IoXCJzdXNwZW5kZWRcIiksXG4gIFNUT1BQRUQ6IFN5bWJvbC5mb3IoXCJzdG9wcGVkXCIpLFxuICBTTEVFUDogU3ltYm9sLmZvcihcInNsZWVwXCIpLFxuICBFWElUOiBTeW1ib2wuZm9yKFwiZXhpdFwiKSxcbiAgTk9NQVRDSDogU3ltYm9sLmZvcihcIm5vX21hdGNoXCIpXG59O1xuXG5mdW5jdGlvbiBpc19zbGVlcCh2YWx1ZSkge1xuICByZXR1cm4gQXJyYXkuaXNBcnJheSh2YWx1ZSkgJiYgdmFsdWVbMF0gPT09IFN0YXRlcy5TTEVFUDtcbn1cblxuZnVuY3Rpb24gaXNfcmVjZWl2ZSh2YWx1ZSkge1xuICByZXR1cm4gQXJyYXkuaXNBcnJheSh2YWx1ZSkgJiYgdmFsdWVbMF0gPT09IFN0YXRlcy5SRUNFSVZFO1xufVxuXG5mdW5jdGlvbiByZWNlaXZlX3RpbWVkX291dCh2YWx1ZSkge1xuICByZXR1cm4gdmFsdWVbMl0gIT0gbnVsbCAmJiB2YWx1ZVsyXSA8IERhdGUubm93KCk7XG59XG5cbmNsYXNzIFByb2Nlc3Mge1xuICBjb25zdHJ1Y3RvcihwaWQsIGZ1bmMsIGFyZ3MsIG1haWxib3gsIHN5c3RlbSkge1xuICAgIHRoaXMucGlkID0gcGlkO1xuICAgIHRoaXMuZnVuYyA9IGZ1bmM7XG4gICAgdGhpcy5hcmdzID0gYXJncztcbiAgICB0aGlzLm1haWxib3ggPSBtYWlsYm94O1xuICAgIHRoaXMuc3lzdGVtID0gc3lzdGVtO1xuICAgIHRoaXMuc3RhdHVzID0gU3RhdGVzLlNUT1BQRUQ7XG4gICAgdGhpcy5kaWN0ID0ge307XG4gICAgdGhpcy5mbGFncyA9IHt9O1xuICAgIHRoaXMubW9uaXRvcnMgPSBbXTtcbiAgfVxuXG4gIHN0YXJ0KCkge1xuICAgIGNvbnN0IGZ1bmN0aW9uX3Njb3BlID0gdGhpcztcbiAgICBsZXQgbWFjaGluZSA9IHRoaXMubWFpbigpO1xuXG4gICAgdGhpcy5zeXN0ZW0uc2NoZWR1bGUoZnVuY3Rpb24gKCkge1xuICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgbWFjaGluZS5uZXh0KCkpO1xuICAgIH0sIHRoaXMucGlkKTtcbiAgfVxuXG4gICptYWluKCkge1xuICAgIGxldCByZXR2YWwgPSBTdGF0ZXMuTk9STUFMO1xuXG4gICAgdHJ5IHtcbiAgICAgIHlpZWxkKiB0aGlzLmZ1bmMuYXBwbHkobnVsbCwgdGhpcy5hcmdzKTtcbiAgICB9IGNhdGNoIChlKSB7XG4gICAgICBjb25zb2xlLmVycm9yKGUpO1xuICAgICAgcmV0dmFsID0gZTtcbiAgICB9XG5cbiAgICB0aGlzLnN5c3RlbS5leGl0KHJldHZhbCk7XG4gIH1cblxuICBwcm9jZXNzX2ZsYWcoZmxhZywgdmFsdWUpIHtcbiAgICBjb25zdCBvbGRfdmFsdWUgPSB0aGlzLmZsYWdzW2ZsYWddO1xuICAgIHRoaXMuZmxhZ3NbZmxhZ10gPSB2YWx1ZTtcbiAgICByZXR1cm4gb2xkX3ZhbHVlO1xuICB9XG5cbiAgaXNfdHJhcHBpbmdfZXhpdHMoKSB7XG4gICAgcmV0dXJuIHRoaXMuZmxhZ3NbU3ltYm9sLmZvcihcInRyYXBfZXhpdFwiKV0gJiYgdGhpcy5mbGFnc1tTeW1ib2wuZm9yKFwidHJhcF9leGl0XCIpXSA9PSB0cnVlO1xuICB9XG5cbiAgc2lnbmFsKHJlYXNvbikge1xuICAgIGlmIChyZWFzb24gIT09IFN0YXRlcy5OT1JNQUwpIHtcbiAgICAgIGNvbnNvbGUuZXJyb3IocmVhc29uKTtcbiAgICB9XG5cbiAgICB0aGlzLnN5c3RlbS5yZW1vdmVfcHJvYyh0aGlzLnBpZCwgcmVhc29uKTtcbiAgfVxuXG4gIHJlY2VpdmUoZnVuKSB7XG4gICAgbGV0IHZhbHVlID0gU3RhdGVzLk5PTUFUQ0g7XG4gICAgbGV0IG1lc3NhZ2VzID0gdGhpcy5tYWlsYm94LmdldCgpO1xuXG4gICAgZm9yIChsZXQgaSA9IDA7IGkgPCBtZXNzYWdlcy5sZW5ndGg7IGkrKykge1xuICAgICAgdHJ5IHtcbiAgICAgICAgdmFsdWUgPSBmdW4obWVzc2FnZXNbaV0pO1xuICAgICAgICBpZiAodmFsdWUgIT09IFN0YXRlcy5OT01BVENIKSB7XG4gICAgICAgICAgdGhpcy5tYWlsYm94LnJlbW92ZUF0KGkpO1xuICAgICAgICAgIGJyZWFrO1xuICAgICAgICB9XG4gICAgICB9IGNhdGNoIChlKSB7XG4gICAgICAgIGlmIChlLmNvbnN0cnVjdG9yLm5hbWUgIT0gXCJNYXRjaEVycm9yXCIpIHtcbiAgICAgICAgICB0aGlzLmV4aXQoZSk7XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICB9XG5cbiAgICByZXR1cm4gdmFsdWU7XG4gIH1cblxuICBydW4obWFjaGluZSwgc3RlcCkge1xuICAgIGNvbnN0IGZ1bmN0aW9uX3Njb3BlID0gdGhpcztcblxuICAgIGlmICghc3RlcC5kb25lKSB7XG4gICAgICBsZXQgdmFsdWUgPSBzdGVwLnZhbHVlO1xuXG4gICAgICBpZiAoaXNfc2xlZXAodmFsdWUpKSB7XG5cbiAgICAgICAgdGhpcy5zeXN0ZW0uZGVsYXkoZnVuY3Rpb24gKCkge1xuICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnN5c3RlbS5zZXRfY3VycmVudChmdW5jdGlvbl9zY29wZS5waWQpO1xuICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnJ1bihtYWNoaW5lLCBtYWNoaW5lLm5leHQoKSk7XG4gICAgICAgIH0sIHZhbHVlWzFdKTtcbiAgICAgIH0gZWxzZSBpZiAoaXNfcmVjZWl2ZSh2YWx1ZSkgJiYgcmVjZWl2ZV90aW1lZF9vdXQodmFsdWUpKSB7XG5cbiAgICAgICAgbGV0IHJlc3VsdCA9IHZhbHVlWzNdKCk7XG5cbiAgICAgICAgdGhpcy5zeXN0ZW0uc2NoZWR1bGUoZnVuY3Rpb24gKCkge1xuICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnN5c3RlbS5zZXRfY3VycmVudChmdW5jdGlvbl9zY29wZS5waWQpO1xuICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnJ1bihtYWNoaW5lLCBtYWNoaW5lLm5leHQocmVzdWx0KSk7XG4gICAgICAgIH0pO1xuICAgICAgfSBlbHNlIGlmIChpc19yZWNlaXZlKHZhbHVlKSkge1xuXG4gICAgICAgIGxldCByZXN1bHQgPSBmdW5jdGlvbl9zY29wZS5yZWNlaXZlKHZhbHVlWzFdKTtcblxuICAgICAgICBpZiAocmVzdWx0ID09PSBTdGF0ZXMuTk9NQVRDSCkge1xuICAgICAgICAgIHRoaXMuc3lzdGVtLnN1c3BlbmQoZnVuY3Rpb24gKCkge1xuICAgICAgICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICAgICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgc3RlcCk7XG4gICAgICAgICAgfSk7XG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgdGhpcy5zeXN0ZW0uc2NoZWR1bGUoZnVuY3Rpb24gKCkge1xuICAgICAgICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICAgICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgbWFjaGluZS5uZXh0KHJlc3VsdCkpO1xuICAgICAgICAgIH0pO1xuICAgICAgICB9XG4gICAgICB9IGVsc2Uge1xuICAgICAgICB0aGlzLnN5c3RlbS5zY2hlZHVsZShmdW5jdGlvbiAoKSB7XG4gICAgICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICAgICAgZnVuY3Rpb25fc2NvcGUucnVuKG1hY2hpbmUsIG1hY2hpbmUubmV4dCh2YWx1ZSkpO1xuICAgICAgICB9KTtcbiAgICAgIH1cbiAgICB9XG4gIH1cbn1cblxuY2xhc3MgUHJvY2Vzc1F1ZXVlIHtcbiAgY29uc3RydWN0b3IocGlkKSB7XG4gICAgdGhpcy5waWQgPSBwaWQ7XG4gICAgdGhpcy50YXNrcyA9IFtdO1xuICB9XG5cbiAgZW1wdHkoKSB7XG4gICAgcmV0dXJuIHRoaXMudGFza3MubGVuZ3RoID09PSAwO1xuICB9XG5cbiAgYWRkKHRhc2spIHtcbiAgICB0aGlzLnRhc2tzLnB1c2godGFzayk7XG4gIH1cblxuICBuZXh0KCkge1xuICAgIHJldHVybiB0aGlzLnRhc2tzLnNoaWZ0KCk7XG4gIH1cbn1cblxuY2xhc3MgU2NoZWR1bGVyIHtcbiAgY29uc3RydWN0b3IodGhyb3R0bGUgPSAwLCByZWR1Y3Rpb25zX3Blcl9wcm9jZXNzID0gOCkge1xuICAgIHRoaXMuaXNSdW5uaW5nID0gZmFsc2U7XG4gICAgdGhpcy5pbnZva2VMYXRlciA9IGZ1bmN0aW9uIChjYWxsYmFjaykge1xuICAgICAgc2V0VGltZW91dChjYWxsYmFjaywgdGhyb3R0bGUpO1xuICAgIH07XG5cbiAgICAvLyBJbiBvdXIgY2FzZSBhIHJlZHVjdGlvbiBpcyBlcXVhbCB0byBhIHRhc2sgY2FsbFxuICAgIC8vIENvbnRyb2xzIGhvdyBtYW55IHRhc2tzIGFyZSBjYWxsZWQgYXQgYSB0aW1lIHBlciBwcm9jZXNzXG4gICAgdGhpcy5yZWR1Y3Rpb25zX3Blcl9wcm9jZXNzID0gcmVkdWN0aW9uc19wZXJfcHJvY2VzcztcbiAgICB0aGlzLnF1ZXVlcyA9IG5ldyBNYXAoKTtcbiAgICB0aGlzLnJ1bigpO1xuICB9XG5cbiAgYWRkVG9RdWV1ZShwaWQsIHRhc2spIHtcbiAgICBpZiAoIXRoaXMucXVldWVzLmhhcyhwaWQpKSB7XG4gICAgICB0aGlzLnF1ZXVlcy5zZXQocGlkLCBuZXcgUHJvY2Vzc1F1ZXVlKHBpZCkpO1xuICAgIH1cblxuICAgIHRoaXMucXVldWVzLmdldChwaWQpLmFkZCh0YXNrKTtcbiAgfVxuXG4gIHJlbW92ZVBpZChwaWQpIHtcbiAgICB0aGlzLmlzUnVubmluZyA9IHRydWU7XG5cbiAgICB0aGlzLnF1ZXVlcy5kZWxldGUocGlkKTtcblxuICAgIHRoaXMuaXNSdW5uaW5nID0gZmFsc2U7XG4gIH1cblxuICBydW4oKSB7XG4gICAgaWYgKHRoaXMuaXNSdW5uaW5nKSB7XG4gICAgICB0aGlzLmludm9rZUxhdGVyKCgpID0+IHtcbiAgICAgICAgdGhpcy5ydW4oKTtcbiAgICAgIH0pO1xuICAgIH0gZWxzZSB7XG4gICAgICBmb3IgKGxldCBbcGlkLCBxdWV1ZV0gb2YgdGhpcy5xdWV1ZXMpIHtcbiAgICAgICAgbGV0IHJlZHVjdGlvbnMgPSAwO1xuICAgICAgICB3aGlsZSAocXVldWUgJiYgIXF1ZXVlLmVtcHR5KCkgJiYgcmVkdWN0aW9ucyA8IHRoaXMucmVkdWN0aW9uc19wZXJfcHJvY2Vzcykge1xuICAgICAgICAgIGxldCB0YXNrID0gcXVldWUubmV4dCgpO1xuICAgICAgICAgIHRoaXMuaXNSdW5uaW5nID0gdHJ1ZTtcblxuICAgICAgICAgIGxldCByZXN1bHQ7XG5cbiAgICAgICAgICB0cnkge1xuICAgICAgICAgICAgcmVzdWx0ID0gdGFzaygpO1xuICAgICAgICAgIH0gY2F0Y2ggKGUpIHtcbiAgICAgICAgICAgIGNvbnNvbGUuZXJyb3IoZSk7XG4gICAgICAgICAgICByZXN1bHQgPSBlO1xuICAgICAgICAgIH1cblxuICAgICAgICAgIHRoaXMuaXNSdW5uaW5nID0gZmFsc2U7XG5cbiAgICAgICAgICBpZiAocmVzdWx0IGluc3RhbmNlb2YgRXJyb3IpIHtcbiAgICAgICAgICAgIHRocm93IHJlc3VsdDtcbiAgICAgICAgICB9XG5cbiAgICAgICAgICByZWR1Y3Rpb25zKys7XG4gICAgICAgIH1cbiAgICAgIH1cblxuICAgICAgdGhpcy5pbnZva2VMYXRlcigoKSA9PiB7XG4gICAgICAgIHRoaXMucnVuKCk7XG4gICAgICB9KTtcbiAgICB9XG4gIH1cblxuICBhZGRUb1NjaGVkdWxlcihwaWQsIHRhc2ssIGR1ZVRpbWUgPSAwKSB7XG4gICAgaWYgKGR1ZVRpbWUgPT09IDApIHtcbiAgICAgIHRoaXMuaW52b2tlTGF0ZXIoKCkgPT4ge1xuICAgICAgICB0aGlzLmFkZFRvUXVldWUocGlkLCB0YXNrKTtcbiAgICAgIH0pO1xuICAgIH0gZWxzZSB7XG4gICAgICBzZXRUaW1lb3V0KCgpID0+IHtcbiAgICAgICAgdGhpcy5hZGRUb1F1ZXVlKHBpZCwgdGFzayk7XG4gICAgICB9LCBkdWVUaW1lKTtcbiAgICB9XG4gIH1cblxuICBzY2hlZHVsZShwaWQsIHRhc2spIHtcbiAgICB0aGlzLmFkZFRvU2NoZWR1bGVyKHBpZCwgKCkgPT4ge1xuICAgICAgdGFzaygpO1xuICAgIH0pO1xuICB9XG5cbiAgc2NoZWR1bGVGdXR1cmUocGlkLCBkdWVUaW1lLCB0YXNrKSB7XG4gICAgdGhpcy5hZGRUb1NjaGVkdWxlcihwaWQsICgpID0+IHtcbiAgICAgIHRhc2soKTtcbiAgICB9LCBkdWVUaW1lKTtcbiAgfVxufVxuXG5sZXQgcHJvY2Vzc19jb3VudGVyID0gLTE7XG5cbmNsYXNzIFBJRCB7XG4gIGNvbnN0cnVjdG9yKCkge1xuICAgIHByb2Nlc3NfY291bnRlciA9IHByb2Nlc3NfY291bnRlciArIDE7XG4gICAgdGhpcy5pZCA9IHByb2Nlc3NfY291bnRlcjtcbiAgfVxuXG4gIHRvU3RyaW5nKCkge1xuICAgIHJldHVybiBcIlBJRCM8MC5cIiArIHRoaXMuaWQgKyBcIi4wPlwiO1xuICB9XG59XG5cbmxldCByZWZfY291bnRlciA9IC0xO1xuXG5jbGFzcyBSZWZlcmVuY2Uge1xuICBjb25zdHJ1Y3RvcigpIHtcbiAgICByZWZfY291bnRlciA9IHJlZl9jb3VudGVyICsgMTtcbiAgICB0aGlzLmlkID0gcmVmX2NvdW50ZXI7XG4gICAgdGhpcy5yZWYgPSBTeW1ib2woKTtcbiAgfVxuXG4gIHRvU3RyaW5nKCkge1xuICAgIHJldHVybiBcIlJlZiM8MC4wLjAuXCIgKyB0aGlzLmlkICsgXCI+XCI7XG4gIH1cbn1cblxuY2xhc3MgUHJvY2Vzc1N5c3RlbSB7XG5cbiAgY29uc3RydWN0b3IoKSB7XG4gICAgdGhpcy5waWRzID0gbmV3IE1hcCgpO1xuICAgIHRoaXMubWFpbGJveGVzID0gbmV3IE1hcCgpO1xuICAgIHRoaXMubmFtZXMgPSBuZXcgTWFwKCk7XG4gICAgdGhpcy5saW5rcyA9IG5ldyBNYXAoKTtcbiAgICB0aGlzLm1vbml0b3JzID0gbmV3IE1hcCgpO1xuXG4gICAgY29uc3QgdGhyb3R0bGUgPSA1OyAvL21zIGJldHdlZW4gc2NoZWR1bGVkIHRhc2tzXG4gICAgdGhpcy5jdXJyZW50X3Byb2Nlc3MgPSBudWxsO1xuICAgIHRoaXMuc2NoZWR1bGVyID0gbmV3IFNjaGVkdWxlcih0aHJvdHRsZSk7XG4gICAgdGhpcy5zdXNwZW5kZWQgPSBuZXcgTWFwKCk7XG5cbiAgICBsZXQgcHJvY2Vzc19zeXN0ZW1fc2NvcGUgPSB0aGlzO1xuICAgIHRoaXMubWFpbl9wcm9jZXNzX3BpZCA9IHRoaXMuc3Bhd24oZnVuY3Rpb24qICgpIHtcbiAgICAgIHdoaWxlICh0cnVlKSB7XG4gICAgICAgIHlpZWxkIHByb2Nlc3Nfc3lzdGVtX3Njb3BlLnNsZWVwKDEwMDAwKTtcbiAgICAgIH1cbiAgICB9KTtcbiAgICB0aGlzLnNldF9jdXJyZW50KHRoaXMubWFpbl9wcm9jZXNzX3BpZCk7XG4gIH1cblxuICBzdGF0aWMgKnJ1bihmdW4sIGFyZ3MsIGNvbnRleHQgPSBudWxsKSB7XG4gICAgaWYgKGZ1bi5jb25zdHJ1Y3Rvci5uYW1lID09PSBcIkdlbmVyYXRvckZ1bmN0aW9uXCIpIHtcbiAgICAgIHJldHVybiB5aWVsZCogZnVuLmFwcGx5KGNvbnRleHQsIGFyZ3MpO1xuICAgIH0gZWxzZSB7XG4gICAgICByZXR1cm4geWllbGQgZnVuLmFwcGx5KGNvbnRleHQsIGFyZ3MpO1xuICAgIH1cbiAgfVxuXG4gIHNwYXduKC4uLmFyZ3MpIHtcbiAgICBpZiAoYXJncy5sZW5ndGggPT09IDEpIHtcbiAgICAgIGxldCBmdW4gPSBhcmdzWzBdO1xuICAgICAgcmV0dXJuIHRoaXMuYWRkX3Byb2MoZnVuLCBbXSwgZmFsc2UpLnBpZDtcbiAgICB9IGVsc2Uge1xuICAgICAgbGV0IG1vZCA9IGFyZ3NbMF07XG4gICAgICBsZXQgZnVuID0gYXJnc1sxXTtcbiAgICAgIGxldCB0aGVfYXJncyA9IGFyZ3NbMl07XG5cbiAgICAgIHJldHVybiB0aGlzLmFkZF9wcm9jKG1vZFtmdW5dLCB0aGVfYXJncywgZmFsc2UsIGZhbHNlKS5waWQ7XG4gICAgfVxuICB9XG5cbiAgc3Bhd25fbGluayguLi5hcmdzKSB7XG4gICAgaWYgKGFyZ3MubGVuZ3RoID09PSAxKSB7XG4gICAgICBsZXQgZnVuID0gYXJnc1swXTtcbiAgICAgIHJldHVybiB0aGlzLmFkZF9wcm9jKGZ1biwgW10sIHRydWUsIGZhbHNlKS5waWQ7XG4gICAgfSBlbHNlIHtcbiAgICAgIGxldCBtb2QgPSBhcmdzWzBdO1xuICAgICAgbGV0IGZ1biA9IGFyZ3NbMV07XG4gICAgICBsZXQgdGhlX2FyZ3MgPSBhcmdzWzJdO1xuXG4gICAgICByZXR1cm4gdGhpcy5hZGRfcHJvYyhtb2RbZnVuXSwgdGhlX2FyZ3MsIHRydWUsIGZhbHNlKS5waWQ7XG4gICAgfVxuICB9XG5cbiAgbGluayhwaWQpIHtcbiAgICB0aGlzLmxpbmtzLmdldCh0aGlzLnBpZCgpKS5hZGQocGlkKTtcbiAgICB0aGlzLmxpbmtzLmdldChwaWQpLmFkZCh0aGlzLnBpZCgpKTtcbiAgfVxuXG4gIHVubGluayhwaWQpIHtcbiAgICB0aGlzLmxpbmtzLmdldCh0aGlzLnBpZCgpKS5kZWxldGUocGlkKTtcbiAgICB0aGlzLmxpbmtzLmdldChwaWQpLmRlbGV0ZSh0aGlzLnBpZCgpKTtcbiAgfVxuXG4gIHNwYXduX21vbml0b3IoLi4uYXJncykge1xuICAgIGlmIChhcmdzLmxlbmd0aCA9PT0gMSkge1xuICAgICAgbGV0IGZ1biA9IGFyZ3NbMF07XG4gICAgICBsZXQgcHJvY2VzcyA9IHRoaXMuYWRkX3Byb2MoZnVuLCBbXSwgZmFsc2UsIHRydWUpO1xuICAgICAgcmV0dXJuIFtwcm9jZXNzLnBpZCwgcHJvY2Vzcy5tb25pdG9yc1swXV07XG4gICAgfSBlbHNlIHtcbiAgICAgIGxldCBtb2QgPSBhcmdzWzBdO1xuICAgICAgbGV0IGZ1biA9IGFyZ3NbMV07XG4gICAgICBsZXQgdGhlX2FyZ3MgPSBhcmdzWzJdO1xuICAgICAgbGV0IHByb2Nlc3MgPSB0aGlzLmFkZF9wcm9jKG1vZFtmdW5dLCB0aGVfYXJncywgZmFsc2UsIHRydWUpO1xuXG4gICAgICByZXR1cm4gW3Byb2Nlc3MucGlkLCBwcm9jZXNzLm1vbml0b3JzWzBdXTtcbiAgICB9XG4gIH1cblxuICBtb25pdG9yKHBpZCkge1xuICAgIGNvbnN0IHJlYWxfcGlkID0gdGhpcy5waWRvZihwaWQpO1xuICAgIGNvbnN0IHJlZiA9IHRoaXMubWFrZV9yZWYoKTtcblxuICAgIGlmIChyZWFsX3BpZCkge1xuXG4gICAgICB0aGlzLm1vbml0b3JzLnNldChyZWYsIHsgJ21vbml0b3InOiB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5waWQsICdtb25pdGVlJzogcmVhbF9waWQgfSk7XG4gICAgICB0aGlzLnBpZHMuZ2V0KHJlYWxfcGlkKS5tb25pdG9ycyhyZWYpO1xuICAgICAgcmV0dXJuIHJlZjtcbiAgICB9IGVsc2Uge1xuICAgICAgdGhpcy5zZW5kKHRoaXMuY3VycmVudF9wcm9jZXNzLnBpZCwgWydET1dOJywgcmVmLCBwaWQsIHJlYWxfcGlkLCBTeW1ib2wuZm9yKCdub3Byb2MnKV0pO1xuICAgICAgcmV0dXJuIHJlZjtcbiAgICB9XG4gIH1cblxuICBkZW1vbml0b3IocmVmKSB7XG4gICAgaWYgKHRoaXMubW9uaXRvci5oYXMocmVmKSkge1xuICAgICAgdGhpcy5tb25pdG9yLmRlbGV0ZShyZWYpO1xuICAgICAgcmV0dXJuIHRydWU7XG4gICAgfVxuXG4gICAgcmV0dXJuIGZhbHNlO1xuICB9XG5cbiAgc2V0X2N1cnJlbnQoaWQpIHtcbiAgICBsZXQgcGlkID0gdGhpcy5waWRvZihpZCk7XG4gICAgaWYgKHBpZCAhPT0gbnVsbCkge1xuICAgICAgdGhpcy5jdXJyZW50X3Byb2Nlc3MgPSB0aGlzLnBpZHMuZ2V0KHBpZCk7XG4gICAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5zdGF0dXMgPSBTdGF0ZXMuUlVOTklORztcbiAgICB9XG4gIH1cblxuICBhZGRfcHJvYyhmdW4sIGFyZ3MsIGxpbmtlZCwgbW9uaXRvcmVkKSB7XG4gICAgbGV0IG5ld3BpZCA9IG5ldyBQSUQoKTtcbiAgICBsZXQgbWFpbGJveCA9IG5ldyBNYWlsYm94KCk7XG4gICAgbGV0IG5ld3Byb2MgPSBuZXcgUHJvY2VzcyhuZXdwaWQsIGZ1biwgYXJncywgbWFpbGJveCwgdGhpcyk7XG5cbiAgICB0aGlzLnBpZHMuc2V0KG5ld3BpZCwgbmV3cHJvYyk7XG4gICAgdGhpcy5tYWlsYm94ZXMuc2V0KG5ld3BpZCwgbWFpbGJveCk7XG4gICAgdGhpcy5saW5rcy5zZXQobmV3cGlkLCBuZXcgU2V0KCkpO1xuXG4gICAgaWYgKGxpbmtlZCkge1xuICAgICAgdGhpcy5saW5rKG5ld3BpZCk7XG4gICAgfVxuXG4gICAgaWYgKG1vbml0b3JlZCkge1xuICAgICAgdGhpcy5tb25pdG9yKG5ld3BpZCk7XG4gICAgfVxuXG4gICAgbmV3cHJvYy5zdGFydCgpO1xuICAgIHJldHVybiBuZXdwcm9jO1xuICB9XG5cbiAgcmVtb3ZlX3Byb2MocGlkLCBleGl0cmVhc29uKSB7XG4gICAgdGhpcy5waWRzLmRlbGV0ZShwaWQpO1xuICAgIHRoaXMudW5yZWdpc3RlcihwaWQpO1xuICAgIHRoaXMuc2NoZWR1bGVyLnJlbW92ZVBpZChwaWQpO1xuXG4gICAgaWYgKHRoaXMubGlua3MuaGFzKHBpZCkpIHtcbiAgICAgIGZvciAobGV0IGxpbmtwaWQgb2YgdGhpcy5saW5rcy5nZXQocGlkKSkge1xuICAgICAgICB0aGlzLmV4aXQobGlua3BpZCwgZXhpdHJlYXNvbik7XG4gICAgICAgIHRoaXMubGlua3MuZ2V0KGxpbmtwaWQpLmRlbGV0ZShwaWQpO1xuICAgICAgfVxuXG4gICAgICB0aGlzLmxpbmtzLmRlbGV0ZShwaWQpO1xuICAgIH1cbiAgfVxuXG4gIHJlZ2lzdGVyKG5hbWUsIHBpZCkge1xuICAgIGlmICghdGhpcy5uYW1lcy5oYXMobmFtZSkpIHtcbiAgICAgIHRoaXMubmFtZXMuc2V0KG5hbWUsIHBpZCk7XG4gICAgfSBlbHNlIHtcbiAgICAgIHRocm93IG5ldyBFcnJvcihcIk5hbWUgaXMgYWxyZWFkeSByZWdpc3RlcmVkIHRvIGFub3RoZXIgcHJvY2Vzc1wiKTtcbiAgICB9XG4gIH1cblxuICB3aGVyZWlzKG5hbWUpIHtcbiAgICByZXR1cm4gdGhpcy5uYW1lcy5oYXMobmFtZSkgPyB0aGlzLm5hbWVzLmdldChuYW1lKSA6IG51bGw7XG4gIH1cblxuICByZWdpc3RlcmVkKCkge1xuICAgIHJldHVybiB0aGlzLm5hbWVzLmtleXMoKTtcbiAgfVxuXG4gIHVucmVnaXN0ZXIocGlkKSB7XG4gICAgZm9yIChsZXQgbmFtZSBvZiB0aGlzLm5hbWVzLmtleXMoKSkge1xuICAgICAgaWYgKHRoaXMubmFtZXMuaGFzKG5hbWUpICYmIHRoaXMubmFtZXMuZ2V0KG5hbWUpID09PSBwaWQpIHtcbiAgICAgICAgdGhpcy5uYW1lcy5kZWxldGUobmFtZSk7XG4gICAgICB9XG4gICAgfVxuICB9XG5cbiAgcGlkKCkge1xuICAgIHJldHVybiB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5waWQ7XG4gIH1cblxuICBwaWRvZihpZCkge1xuICAgIGlmIChpZCBpbnN0YW5jZW9mIFBJRCkge1xuICAgICAgcmV0dXJuIHRoaXMucGlkcy5oYXMoaWQpID8gaWQgOiBudWxsO1xuICAgIH0gZWxzZSBpZiAoaWQgaW5zdGFuY2VvZiBQcm9jZXNzKSB7XG4gICAgICByZXR1cm4gaWQucGlkO1xuICAgIH0gZWxzZSB7XG4gICAgICBsZXQgcGlkID0gdGhpcy53aGVyZWlzKGlkKTtcbiAgICAgIGlmIChwaWQgPT09IG51bGwpIHRocm93IFwiUHJvY2VzcyBuYW1lIG5vdCByZWdpc3RlcmVkOiBcIiArIGlkICsgXCIgKFwiICsgdHlwZW9mIGlkICsgXCIpXCI7XG4gICAgICByZXR1cm4gcGlkO1xuICAgIH1cbiAgfVxuXG4gIHNlbmQoaWQsIG1zZykge1xuICAgIGNvbnN0IHBpZCA9IHRoaXMucGlkb2YoaWQpO1xuXG4gICAgaWYgKHBpZCkge1xuICAgICAgdGhpcy5tYWlsYm94ZXMuZ2V0KHBpZCkuZGVsaXZlcihtc2cpO1xuXG4gICAgICBpZiAodGhpcy5zdXNwZW5kZWQuaGFzKHBpZCkpIHtcbiAgICAgICAgbGV0IGZ1biA9IHRoaXMuc3VzcGVuZGVkLmdldChwaWQpO1xuICAgICAgICB0aGlzLnN1c3BlbmRlZC5kZWxldGUocGlkKTtcbiAgICAgICAgdGhpcy5zY2hlZHVsZShmdW4pO1xuICAgICAgfVxuICAgIH1cblxuICAgIHJldHVybiBtc2c7XG4gIH1cblxuICByZWNlaXZlKGZ1biwgdGltZW91dCA9IDAsIHRpbWVvdXRGbiA9ICgpID0+IHRydWUpIHtcbiAgICBsZXQgRGF0ZVRpbWVvdXQgPSBudWxsO1xuXG4gICAgaWYgKHRpbWVvdXQgPT09IDAgfHwgdGltZW91dCA9PT0gSW5maW5pdHkpIHtcbiAgICAgIERhdGVUaW1lb3V0ID0gbnVsbDtcbiAgICB9IGVsc2Uge1xuICAgICAgRGF0ZVRpbWVvdXQgPSBEYXRlLm5vdygpICsgdGltZW91dDtcbiAgICB9XG5cbiAgICByZXR1cm4gW1N0YXRlcy5SRUNFSVZFLCBmdW4sIERhdGVUaW1lb3V0LCB0aW1lb3V0Rm5dO1xuICB9XG5cbiAgc2xlZXAoZHVyYXRpb24pIHtcbiAgICByZXR1cm4gW1N0YXRlcy5TTEVFUCwgZHVyYXRpb25dO1xuICB9XG5cbiAgc3VzcGVuZChmdW4pIHtcbiAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5zdGF0dXMgPSBTdGF0ZXMuU1VTUEVOREVEO1xuICAgIHRoaXMuc3VzcGVuZGVkLnNldCh0aGlzLmN1cnJlbnRfcHJvY2Vzcy5waWQsIGZ1bik7XG4gIH1cblxuICBkZWxheShmdW4sIHRpbWUpIHtcbiAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5zdGF0dXMgPSBTdGF0ZXMuU0xFRVBJTkc7XG4gICAgdGhpcy5zY2hlZHVsZXIuc2NoZWR1bGVGdXR1cmUodGhpcy5jdXJyZW50X3Byb2Nlc3MucGlkLCB0aW1lLCBmdW4pO1xuICB9XG5cbiAgc2NoZWR1bGUoZnVuLCBwaWQpIHtcbiAgICBjb25zdCB0aGVfcGlkID0gcGlkICE9IG51bGwgPyBwaWQgOiB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5waWQ7XG4gICAgdGhpcy5zY2hlZHVsZXIuc2NoZWR1bGUodGhlX3BpZCwgZnVuKTtcbiAgfVxuXG4gIGV4aXQob25lLCB0d28pIHtcbiAgICBsZXQgcGlkID0gbnVsbDtcbiAgICBsZXQgcmVhc29uID0gbnVsbDtcbiAgICBsZXQgcHJvY2VzcyA9IG51bGw7XG5cbiAgICBpZiAodHdvKSB7XG4gICAgICBsZXQgcGlkID0gb25lO1xuICAgICAgbGV0IHJlYXNvbiA9IHR3bztcbiAgICAgIGxldCBwcm9jZXNzID0gdGhpcy5waWRzLmdldCh0aGlzLnBpZG9mKHBpZCkpO1xuXG4gICAgICBpZiAocHJvY2VzcyAmJiBwcm9jZXNzLmlzX3RyYXBwaW5nX2V4aXRzKCkgfHwgcmVhc29uID09PSBTdGF0ZXMuS0lMTCB8fCByZWFzb24gPT09IFN0YXRlcy5OT1JNQUwpIHtcbiAgICAgICAgdGhpcy5tYWlsYm94ZXMuZ2V0KHByb2Nlc3MucGlkKS5kZWxpdmVyKFtTdGF0ZXMuRVhJVCwgdGhpcy5waWQoKSwgcmVhc29uXSk7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICBwcm9jZXNzLnNpZ25hbChyZWFzb24pO1xuICAgICAgfVxuICAgIH0gZWxzZSB7XG4gICAgICBsZXQgcGlkID0gdGhpcy5jdXJyZW50X3Byb2Nlc3MucGlkO1xuICAgICAgbGV0IHJlYXNvbiA9IG9uZTtcbiAgICAgIGxldCBwcm9jZXNzID0gdGhpcy5jdXJyZW50X3Byb2NlcztcbiAgICAgIHByb2Nlc3Muc2lnbmFsKHJlYXNvbik7XG4gICAgfVxuXG4gICAgZm9yIChsZXQgcmVmIGluIHByb2Nlc3MubW9uaXRvcnMpIHtcbiAgICAgIGxldCBtb25zID0gdGhpcy5tb25pdG9ycy5nZXQocmVmKTtcbiAgICAgIHRoaXMuc2VuZChtb25zWydtb25pdG9yJ10sIFsnRE9XTicsIHJlZiwgbW9uc1snbW9uaXRlZSddLCBtb25zWydtb25pdGVlJ10sIHJlYXNvbl0pO1xuICAgIH1cbiAgfVxuXG4gIGVycm9yKHJlYXNvbikge1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLnNpZ25hbChyZWFzb24pO1xuICB9XG5cbiAgcHJvY2Vzc19mbGFnKC4uLmFyZ3MpIHtcbiAgICBpZiAoYXJncy5sZW5ndGggPT0gMikge1xuICAgICAgY29uc3QgZmxhZyA9IGFyZ3NbMF07XG4gICAgICBjb25zdCB2YWx1ZSA9IGFyZ3NbMV07XG4gICAgICByZXR1cm4gdGhpcy5jdXJyZW50X3Byb2Nlc3MucHJvY2Vzc19mbGFnKGZsYWcsIHZhbHVlKTtcbiAgICB9IGVsc2Uge1xuICAgICAgY29uc3QgcGlkID0gdGhpcy5waWRvZihhcmdzWzBdKTtcbiAgICAgIGNvbnN0IGZsYWcgPSBhcmdzWzFdO1xuICAgICAgY29uc3QgdmFsdWUgPSBhcmdzWzJdO1xuICAgICAgcmV0dXJuIHRoaXMucGlkcy5nZXQocGlkKS5wcm9jZXNzX2ZsYWcoZmxhZywgdmFsdWUpO1xuICAgIH1cbiAgfVxuXG4gIHB1dChrZXksIHZhbHVlKSB7XG4gICAgdGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdFtrZXldID0gdmFsdWU7XG4gIH1cblxuICBnZXRfcHJvY2Vzc19kaWN0KCkge1xuICAgIHJldHVybiB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0O1xuICB9XG5cbiAgZ2V0KGtleSwgZGVmYXVsdF92YWx1ZSA9IG51bGwpIHtcbiAgICBpZiAoa2V5IGluIHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3QpIHtcbiAgICAgIHJldHVybiB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0W2tleV07XG4gICAgfSBlbHNlIHtcbiAgICAgIHJldHVybiBkZWZhdWx0X3ZhbHVlO1xuICAgIH1cbiAgfVxuXG4gIGdldF9rZXlzKHZhbHVlKSB7XG4gICAgaWYgKHZhbHVlKSB7XG4gICAgICBsZXQga2V5cyA9IFtdO1xuXG4gICAgICBmb3IgKGxldCBrZXkgb2YgT2JqZWN0LmtleXModGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdCkpIHtcbiAgICAgICAgaWYgKHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3Rba2V5XSA9PT0gdmFsdWUpIHtcbiAgICAgICAgICBrZXlzLnB1c2goa2V5KTtcbiAgICAgICAgfVxuICAgICAgfVxuXG4gICAgICByZXR1cm4ga2V5cztcbiAgICB9XG5cbiAgICByZXR1cm4gT2JqZWN0LmtleXModGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdCk7XG4gIH1cblxuICBlcmFzZShrZXkpIHtcbiAgICBpZiAoa2V5ICE9IG51bGwpIHtcbiAgICAgIGRlbGV0ZSB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0W2tleV07XG4gICAgfSBlbHNlIHtcbiAgICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3QgPSB7fTtcbiAgICB9XG4gIH1cblxuICBpc19hbGl2ZShwaWQpIHtcbiAgICBjb25zdCByZWFsX3BpZCA9IHRoaXMucGlkb2YocGlkKTtcbiAgICByZXR1cm4gcmVhbF9waWQgIT0gbnVsbDtcbiAgfVxuXG4gIGxpc3QoKSB7XG4gICAgcmV0dXJuIHRoaXMucGlkcy5rZXlzKCk7XG4gIH1cblxuICBtYWtlX3JlZigpIHtcbiAgICByZXR1cm4gbmV3IFJlZmVyZW5jZSgpO1xuICB9XG59XG5cbnZhciBpbmRleCA9IHtcbiAgUHJvY2Vzc1N5c3RlbVxufTtcblxuZXhwb3J0IGRlZmF1bHQgaW5kZXg7Il0sInNvdXJjZVJvb3QiOiIvc291cmNlLyJ9
