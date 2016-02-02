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
      this.flags[flag] = value;
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
          this.exit(e);
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

const MAX_REDUCTIONS_PER_PROCESS = 8;

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

    _classCallCheck(this, Scheduler);

    this.isRunning = false;
    this.invokeLater = function (callback) {
      setTimeout(callback, throttle);
    };
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
          while (queue && !queue.empty() && reductions < MAX_REDUCTIONS_PER_PROCESS) {
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

let ProcessSystem = (function () {
  function ProcessSystem() {
    _classCallCheck(this, ProcessSystem);

    this.pids = new Map();
    this.mailboxes = new Map();
    this.names = new Map();
    this.links = new Map();

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
      } else if (args.length === 3) {
        let mod = args[0];
        let fun = args[1];
        let the_args = args[2];

        return this.add_proc(mod[fun], the_args, false).pid;
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
        return this.add_proc(fun, [], true).pid;
      } else if (args.length === 3) {
        let mod = args[0];
        let fun = args[1];
        let the_args = args[2];

        return this.add_proc(mod[fun], the_args, true).pid;
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
    value: function add_proc(fun, args, linked) {
      let newpid = new PID();
      let mailbox = new Mailbox();
      let newproc = new Process(newpid, fun, args, mailbox, this);

      this.pids.set(newpid, newproc);
      this.mailboxes.set(newpid, mailbox);
      this.links.set(newpid, new Set());

      if (linked) {
        this.link(newpid);
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
    key: "registered",
    value: function registered(name) {
      return this.names.has(name) ? this.names.get(name) : null;
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
        let pid = this.registered(id);
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
        let reason = one;
        this.current_process.signal(reason);
      }
    }
  }, {
    key: "error",
    value: function error(reason) {
      this.current_process.signal(reason);
    }
  }, {
    key: "process_flag",
    value: function process_flag(flag, value) {
      this.current_process.process_flag(flag, value);
    }
  }, {
    key: "put",
    value: function put(key, value) {
      this.current_process.dict[key] = value;
    }
  }, {
    key: "get",
    value: function get(key) {
      if (key != null) {
        return this.current_process.dict[key];
      } else {
        return this.current_process.dict;
      }
    }
  }, {
    key: "get_keys",
    value: function get_keys() {
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
  }], [{
    key: "run",
    value: function* run(fun, args) {
      let context = arguments.length <= 2 || arguments[2] === undefined ? null : arguments[2];

      if (fun.constructor.name === "GeneratorFunction") {
        return yield* fun.apply(context, args);
      } else {
        return fun.apply(context, args);
      }
    }
  }]);

  return ProcessSystem;
})();

function start(module, args) {
  return [Symbol.for("ok"), self.system.spawn(start_process(module, args))];
}

function start_link(module, args) {
  return [Symbol.for("ok"), self.system.spawn_link(start_process(module, args))];
}

function start_process(module, args) {
  return function* () {
    var _module$init$apply = module.init.apply(null, [args]);

    var _module$init$apply2 = _slicedToArray(_module$init$apply, 2);

    let ok = _module$init$apply2[0];
    let state = _module$init$apply2[1];

    yield self.system.put("state", state);

    try {
      while (true) {
        yield self.system.receive(function (args) {
          let command = args[0];

          switch (command) {
            case "call":
              var request = args[1];
              var sender = args[2];

              var _module$handle_call = module.handle_call(request, sender, self.system.get("state"));

              var _module$handle_call2 = _slicedToArray(_module$handle_call, 3);

              var reply = _module$handle_call2[0];
              var response = _module$handle_call2[1];
              var new_state = _module$handle_call2[2];

              self.system.put("state", new_state);

              self.system.send(sender, response);
              break;

            case "cast":
              var request = args[1];
              var sender = args[2];

              var _module$handle_cast = module.handle_cast(request, self.system.get("state"));

              var _module$handle_cast2 = _slicedToArray(_module$handle_cast, 2);

              var reply = _module$handle_cast2[0];
              var new_state = _module$handle_cast2[1];

              self.system.put("state", new_state);
              self.system.send(args[2], Symbol.for("ok"));

              break;

            case "stop":
              throw "stop";
          }
        });
      }
    } catch (e) {
      if (e !== "stop") {
        throw e;
      }
    }
  };
}

function* call(server, request) {
  self.system.send(server, ["call", request, self.system.pid()]);

  return yield self.system.receive(function (args) {
    return args;
  });
}

function* cast(server, request) {
  self.system.send(server, ["cast", request, self.system.pid()]);

  return yield self.system.receive(function (args) {
    return args;
  });
}

function stop(server) {
  self.system.send(server, ["stop"]);
}

var gen_server = { start: start, start_link: start_link, call: call, cast: cast, stop: stop };

exports.ProcessSystem = ProcessSystem;
exports.GenServer = gen_server;
//# sourceMappingURL=data:application/json;base64,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Qzs7UUFFaEQsYUFBYSxHQUFiLGFBQWE7UUFBZ0IsU0FBUyxHQUF2QixVQUFVIiwiZmlsZSI6ImluZGV4LmpzIiwic291cmNlc0NvbnRlbnQiOlsiY2xhc3MgTWFpbGJveCB7XG5cbiAgY29uc3RydWN0b3IoKSB7XG4gICAgdGhpcy5tZXNzYWdlcyA9IFtdO1xuICB9XG5cbiAgZGVsaXZlcihtZXNzYWdlKSB7XG4gICAgdGhpcy5tZXNzYWdlcy5wdXNoKG1lc3NhZ2UpO1xuICAgIHJldHVybiBtZXNzYWdlO1xuICB9XG5cbiAgZ2V0KCkge1xuICAgIHJldHVybiB0aGlzLm1lc3NhZ2VzO1xuICB9XG5cbiAgaXNFbXB0eSgpIHtcbiAgICByZXR1cm4gdGhpcy5tZXNzYWdlcy5sZW5ndGggPT09IDA7XG4gIH1cblxuICByZW1vdmVBdChpbmRleCkge1xuICAgIHRoaXMubWVzc2FnZXMuc3BsaWNlKGluZGV4LCAxKTtcbiAgfVxufVxuXG52YXIgU3RhdGVzID0ge1xuICBOT1JNQUw6IFN5bWJvbC5mb3IoXCJub3JtYWxcIiksXG4gIEtJTEw6IFN5bWJvbC5mb3IoXCJraWxsXCIpLFxuICBTVVNQRU5EOiBTeW1ib2wuZm9yKFwic3VzcGVuZFwiKSxcbiAgQ09OVElOVUU6IFN5bWJvbC5mb3IoXCJjb250aW51ZVwiKSxcbiAgUkVDRUlWRTogU3ltYm9sLmZvcihcInJlY2VpdmVcIiksXG4gIFNFTkQ6IFN5bWJvbC5mb3IoXCJzZW5kXCIpLFxuICBTTEVFUElORzogU3ltYm9sLmZvcihcInNsZWVwaW5nXCIpLFxuICBSVU5OSU5HOiBTeW1ib2wuZm9yKFwicnVubmluZ1wiKSxcbiAgU1VTUEVOREVEOiBTeW1ib2wuZm9yKFwic3VzcGVuZGVkXCIpLFxuICBTVE9QUEVEOiBTeW1ib2wuZm9yKFwic3RvcHBlZFwiKSxcbiAgU0xFRVA6IFN5bWJvbC5mb3IoXCJzbGVlcFwiKSxcbiAgRVhJVDogU3ltYm9sLmZvcihcImV4aXRcIiksXG4gIE5PTUFUQ0g6IFN5bWJvbC5mb3IoXCJub19tYXRjaFwiKVxufTtcblxuZnVuY3Rpb24gaXNfc2xlZXAodmFsdWUpIHtcbiAgcmV0dXJuIEFycmF5LmlzQXJyYXkodmFsdWUpICYmIHZhbHVlWzBdID09PSBTdGF0ZXMuU0xFRVA7XG59XG5cbmZ1bmN0aW9uIGlzX3JlY2VpdmUodmFsdWUpIHtcbiAgcmV0dXJuIEFycmF5LmlzQXJyYXkodmFsdWUpICYmIHZhbHVlWzBdID09PSBTdGF0ZXMuUkVDRUlWRTtcbn1cblxuZnVuY3Rpb24gcmVjZWl2ZV90aW1lZF9vdXQodmFsdWUpIHtcbiAgcmV0dXJuIHZhbHVlWzJdICE9IG51bGwgJiYgdmFsdWVbMl0gPCBEYXRlLm5vdygpO1xufVxuXG5jbGFzcyBQcm9jZXNzIHtcblxuICBjb25zdHJ1Y3RvcihwaWQsIGZ1bmMsIGFyZ3MsIG1haWxib3gsIHN5c3RlbSkge1xuICAgIHRoaXMucGlkID0gcGlkO1xuICAgIHRoaXMuZnVuYyA9IGZ1bmM7XG4gICAgdGhpcy5hcmdzID0gYXJncztcbiAgICB0aGlzLm1haWxib3ggPSBtYWlsYm94O1xuICAgIHRoaXMuc3lzdGVtID0gc3lzdGVtO1xuICAgIHRoaXMuc3RhdHVzID0gU3RhdGVzLlNUT1BQRUQ7XG4gICAgdGhpcy5kaWN0ID0ge307XG4gICAgdGhpcy5mbGFncyA9IHt9O1xuICB9XG5cbiAgc3RhcnQoKSB7XG4gICAgY29uc3QgZnVuY3Rpb25fc2NvcGUgPSB0aGlzO1xuICAgIGxldCBtYWNoaW5lID0gdGhpcy5tYWluKCk7XG5cbiAgICB0aGlzLnN5c3RlbS5zY2hlZHVsZShmdW5jdGlvbiAoKSB7XG4gICAgICBmdW5jdGlvbl9zY29wZS5zeXN0ZW0uc2V0X2N1cnJlbnQoZnVuY3Rpb25fc2NvcGUucGlkKTtcbiAgICAgIGZ1bmN0aW9uX3Njb3BlLnJ1bihtYWNoaW5lLCBtYWNoaW5lLm5leHQoKSk7XG4gICAgfSwgdGhpcy5waWQpO1xuICB9XG5cbiAgKm1haW4oKSB7XG4gICAgbGV0IHJldHZhbCA9IFN0YXRlcy5OT1JNQUw7XG5cbiAgICB0cnkge1xuICAgICAgeWllbGQqIHRoaXMuZnVuYy5hcHBseShudWxsLCB0aGlzLmFyZ3MpO1xuICAgIH0gY2F0Y2ggKGUpIHtcbiAgICAgIGNvbnNvbGUuZXJyb3IoZSk7XG4gICAgICByZXR2YWwgPSBlO1xuICAgIH1cblxuICAgIHRoaXMuc3lzdGVtLmV4aXQocmV0dmFsKTtcbiAgfVxuXG4gIHByb2Nlc3NfZmxhZyhmbGFnLCB2YWx1ZSkge1xuICAgIHRoaXMuZmxhZ3NbZmxhZ10gPSB2YWx1ZTtcbiAgfVxuXG4gIGlzX3RyYXBwaW5nX2V4aXRzKCkge1xuICAgIHJldHVybiB0aGlzLmZsYWdzW1N5bWJvbC5mb3IoXCJ0cmFwX2V4aXRcIildICYmIHRoaXMuZmxhZ3NbU3ltYm9sLmZvcihcInRyYXBfZXhpdFwiKV0gPT0gdHJ1ZTtcbiAgfVxuXG4gIHNpZ25hbChyZWFzb24pIHtcbiAgICBpZiAocmVhc29uICE9PSBTdGF0ZXMuTk9STUFMKSB7XG4gICAgICBjb25zb2xlLmVycm9yKHJlYXNvbik7XG4gICAgfVxuXG4gICAgdGhpcy5zeXN0ZW0ucmVtb3ZlX3Byb2ModGhpcy5waWQsIHJlYXNvbik7XG4gIH1cblxuICByZWNlaXZlKGZ1bikge1xuICAgIGxldCB2YWx1ZSA9IFN0YXRlcy5OT01BVENIO1xuICAgIGxldCBtZXNzYWdlcyA9IHRoaXMubWFpbGJveC5nZXQoKTtcblxuICAgIGZvciAobGV0IGkgPSAwOyBpIDwgbWVzc2FnZXMubGVuZ3RoOyBpKyspIHtcbiAgICAgIHRyeSB7XG4gICAgICAgIHZhbHVlID0gZnVuKG1lc3NhZ2VzW2ldKTtcbiAgICAgICAgaWYgKHZhbHVlICE9PSBTdGF0ZXMuTk9NQVRDSCkge1xuICAgICAgICAgIHRoaXMubWFpbGJveC5yZW1vdmVBdChpKTtcbiAgICAgICAgICBicmVhaztcbiAgICAgICAgfVxuICAgICAgfSBjYXRjaCAoZSkge1xuICAgICAgICB0aGlzLmV4aXQoZSk7XG4gICAgICB9XG4gICAgfVxuXG4gICAgcmV0dXJuIHZhbHVlO1xuICB9XG5cbiAgcnVuKG1hY2hpbmUsIHN0ZXApIHtcbiAgICBjb25zdCBmdW5jdGlvbl9zY29wZSA9IHRoaXM7XG5cbiAgICBpZiAoIXN0ZXAuZG9uZSkge1xuICAgICAgbGV0IHZhbHVlID0gc3RlcC52YWx1ZTtcblxuICAgICAgaWYgKGlzX3NsZWVwKHZhbHVlKSkge1xuXG4gICAgICAgIHRoaXMuc3lzdGVtLmRlbGF5KGZ1bmN0aW9uICgpIHtcbiAgICAgICAgICBmdW5jdGlvbl9zY29wZS5zeXN0ZW0uc2V0X2N1cnJlbnQoZnVuY3Rpb25fc2NvcGUucGlkKTtcbiAgICAgICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgbWFjaGluZS5uZXh0KCkpO1xuICAgICAgICB9LCB2YWx1ZVsxXSk7XG4gICAgICB9IGVsc2UgaWYgKGlzX3JlY2VpdmUodmFsdWUpICYmIHJlY2VpdmVfdGltZWRfb3V0KHZhbHVlKSkge1xuXG4gICAgICAgIGxldCByZXN1bHQgPSB2YWx1ZVszXSgpO1xuXG4gICAgICAgIHRoaXMuc3lzdGVtLnNjaGVkdWxlKGZ1bmN0aW9uICgpIHtcbiAgICAgICAgICBmdW5jdGlvbl9zY29wZS5zeXN0ZW0uc2V0X2N1cnJlbnQoZnVuY3Rpb25fc2NvcGUucGlkKTtcbiAgICAgICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgbWFjaGluZS5uZXh0KHJlc3VsdCkpO1xuICAgICAgICB9KTtcbiAgICAgIH0gZWxzZSBpZiAoaXNfcmVjZWl2ZSh2YWx1ZSkpIHtcblxuICAgICAgICBsZXQgcmVzdWx0ID0gZnVuY3Rpb25fc2NvcGUucmVjZWl2ZSh2YWx1ZVsxXSk7XG5cbiAgICAgICAgaWYgKHJlc3VsdCA9PT0gU3RhdGVzLk5PTUFUQ0gpIHtcbiAgICAgICAgICB0aGlzLnN5c3RlbS5zdXNwZW5kKGZ1bmN0aW9uICgpIHtcbiAgICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnN5c3RlbS5zZXRfY3VycmVudChmdW5jdGlvbl9zY29wZS5waWQpO1xuICAgICAgICAgICAgZnVuY3Rpb25fc2NvcGUucnVuKG1hY2hpbmUsIHN0ZXApO1xuICAgICAgICAgIH0pO1xuICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgIHRoaXMuc3lzdGVtLnNjaGVkdWxlKGZ1bmN0aW9uICgpIHtcbiAgICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnN5c3RlbS5zZXRfY3VycmVudChmdW5jdGlvbl9zY29wZS5waWQpO1xuICAgICAgICAgICAgZnVuY3Rpb25fc2NvcGUucnVuKG1hY2hpbmUsIG1hY2hpbmUubmV4dChyZXN1bHQpKTtcbiAgICAgICAgICB9KTtcbiAgICAgICAgfVxuICAgICAgfSBlbHNlIHtcbiAgICAgICAgdGhpcy5zeXN0ZW0uc2NoZWR1bGUoZnVuY3Rpb24gKCkge1xuICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnN5c3RlbS5zZXRfY3VycmVudChmdW5jdGlvbl9zY29wZS5waWQpO1xuICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnJ1bihtYWNoaW5lLCBtYWNoaW5lLm5leHQodmFsdWUpKTtcbiAgICAgICAgfSk7XG4gICAgICB9XG4gICAgfVxuICB9XG59XG5cbmNvbnN0IE1BWF9SRURVQ1RJT05TX1BFUl9QUk9DRVNTID0gODtcblxuY2xhc3MgUHJvY2Vzc1F1ZXVlIHtcbiAgY29uc3RydWN0b3IocGlkKSB7XG4gICAgdGhpcy5waWQgPSBwaWQ7XG4gICAgdGhpcy50YXNrcyA9IFtdO1xuICB9XG5cbiAgZW1wdHkoKSB7XG4gICAgcmV0dXJuIHRoaXMudGFza3MubGVuZ3RoID09PSAwO1xuICB9XG5cbiAgYWRkKHRhc2spIHtcbiAgICB0aGlzLnRhc2tzLnB1c2godGFzayk7XG4gIH1cblxuICBuZXh0KCkge1xuICAgIHJldHVybiB0aGlzLnRhc2tzLnNoaWZ0KCk7XG4gIH1cbn1cblxuY2xhc3MgU2NoZWR1bGVyIHtcbiAgY29uc3RydWN0b3IodGhyb3R0bGUgPSAwKSB7XG4gICAgdGhpcy5pc1J1bm5pbmcgPSBmYWxzZTtcbiAgICB0aGlzLmludm9rZUxhdGVyID0gZnVuY3Rpb24gKGNhbGxiYWNrKSB7XG4gICAgICBzZXRUaW1lb3V0KGNhbGxiYWNrLCB0aHJvdHRsZSk7XG4gICAgfTtcbiAgICB0aGlzLnF1ZXVlcyA9IG5ldyBNYXAoKTtcbiAgICB0aGlzLnJ1bigpO1xuICB9XG5cbiAgYWRkVG9RdWV1ZShwaWQsIHRhc2spIHtcbiAgICBpZiAoIXRoaXMucXVldWVzLmhhcyhwaWQpKSB7XG4gICAgICB0aGlzLnF1ZXVlcy5zZXQocGlkLCBuZXcgUHJvY2Vzc1F1ZXVlKHBpZCkpO1xuICAgIH1cblxuICAgIHRoaXMucXVldWVzLmdldChwaWQpLmFkZCh0YXNrKTtcbiAgfVxuXG4gIHJlbW92ZVBpZChwaWQpIHtcbiAgICB0aGlzLmlzUnVubmluZyA9IHRydWU7XG5cbiAgICB0aGlzLnF1ZXVlcy5kZWxldGUocGlkKTtcblxuICAgIHRoaXMuaXNSdW5uaW5nID0gZmFsc2U7XG4gIH1cblxuICBydW4oKSB7XG4gICAgaWYgKHRoaXMuaXNSdW5uaW5nKSB7XG4gICAgICB0aGlzLmludm9rZUxhdGVyKCgpID0+IHtcbiAgICAgICAgdGhpcy5ydW4oKTtcbiAgICAgIH0pO1xuICAgIH0gZWxzZSB7XG4gICAgICBmb3IgKGxldCBbcGlkLCBxdWV1ZV0gb2YgdGhpcy5xdWV1ZXMpIHtcbiAgICAgICAgbGV0IHJlZHVjdGlvbnMgPSAwO1xuICAgICAgICB3aGlsZSAocXVldWUgJiYgIXF1ZXVlLmVtcHR5KCkgJiYgcmVkdWN0aW9ucyA8IE1BWF9SRURVQ1RJT05TX1BFUl9QUk9DRVNTKSB7XG4gICAgICAgICAgbGV0IHRhc2sgPSBxdWV1ZS5uZXh0KCk7XG4gICAgICAgICAgdGhpcy5pc1J1bm5pbmcgPSB0cnVlO1xuXG4gICAgICAgICAgbGV0IHJlc3VsdDtcblxuICAgICAgICAgIHRyeSB7XG4gICAgICAgICAgICByZXN1bHQgPSB0YXNrKCk7XG4gICAgICAgICAgfSBjYXRjaCAoZSkge1xuICAgICAgICAgICAgY29uc29sZS5lcnJvcihlKTtcbiAgICAgICAgICAgIHJlc3VsdCA9IGU7XG4gICAgICAgICAgfVxuXG4gICAgICAgICAgdGhpcy5pc1J1bm5pbmcgPSBmYWxzZTtcblxuICAgICAgICAgIGlmIChyZXN1bHQgaW5zdGFuY2VvZiBFcnJvcikge1xuICAgICAgICAgICAgdGhyb3cgcmVzdWx0O1xuICAgICAgICAgIH1cblxuICAgICAgICAgIHJlZHVjdGlvbnMrKztcbiAgICAgICAgfVxuICAgICAgfVxuXG4gICAgICB0aGlzLmludm9rZUxhdGVyKCgpID0+IHtcbiAgICAgICAgdGhpcy5ydW4oKTtcbiAgICAgIH0pO1xuICAgIH1cbiAgfVxuXG4gIGFkZFRvU2NoZWR1bGVyKHBpZCwgdGFzaywgZHVlVGltZSA9IDApIHtcbiAgICBpZiAoZHVlVGltZSA9PT0gMCkge1xuICAgICAgdGhpcy5pbnZva2VMYXRlcigoKSA9PiB7XG4gICAgICAgIHRoaXMuYWRkVG9RdWV1ZShwaWQsIHRhc2spO1xuICAgICAgfSk7XG4gICAgfSBlbHNlIHtcbiAgICAgIHNldFRpbWVvdXQoKCkgPT4ge1xuICAgICAgICB0aGlzLmFkZFRvUXVldWUocGlkLCB0YXNrKTtcbiAgICAgIH0sIGR1ZVRpbWUpO1xuICAgIH1cbiAgfVxuXG4gIHNjaGVkdWxlKHBpZCwgdGFzaykge1xuICAgIHRoaXMuYWRkVG9TY2hlZHVsZXIocGlkLCAoKSA9PiB7XG4gICAgICB0YXNrKCk7XG4gICAgfSk7XG4gIH1cblxuICBzY2hlZHVsZUZ1dHVyZShwaWQsIGR1ZVRpbWUsIHRhc2spIHtcbiAgICB0aGlzLmFkZFRvU2NoZWR1bGVyKHBpZCwgKCkgPT4ge1xuICAgICAgdGFzaygpO1xuICAgIH0sIGR1ZVRpbWUpO1xuICB9XG59XG5cbmxldCBwcm9jZXNzX2NvdW50ZXIgPSAtMTtcblxuY2xhc3MgUElEIHtcbiAgY29uc3RydWN0b3IoKSB7XG4gICAgcHJvY2Vzc19jb3VudGVyID0gcHJvY2Vzc19jb3VudGVyICsgMTtcbiAgICB0aGlzLmlkID0gcHJvY2Vzc19jb3VudGVyO1xuICB9XG5cbiAgdG9TdHJpbmcoKSB7XG4gICAgcmV0dXJuIFwiUElEIzwwLlwiICsgdGhpcy5pZCArIFwiLjA+XCI7XG4gIH1cbn1cblxuY2xhc3MgUHJvY2Vzc1N5c3RlbSB7XG5cbiAgY29uc3RydWN0b3IoKSB7XG4gICAgdGhpcy5waWRzID0gbmV3IE1hcCgpO1xuICAgIHRoaXMubWFpbGJveGVzID0gbmV3IE1hcCgpO1xuICAgIHRoaXMubmFtZXMgPSBuZXcgTWFwKCk7XG4gICAgdGhpcy5saW5rcyA9IG5ldyBNYXAoKTtcblxuICAgIGNvbnN0IHRocm90dGxlID0gNTsgLy9tcyBiZXR3ZWVuIHNjaGVkdWxlZCB0YXNrc1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzID0gbnVsbDtcbiAgICB0aGlzLnNjaGVkdWxlciA9IG5ldyBTY2hlZHVsZXIodGhyb3R0bGUpO1xuICAgIHRoaXMuc3VzcGVuZGVkID0gbmV3IE1hcCgpO1xuXG4gICAgbGV0IHByb2Nlc3Nfc3lzdGVtX3Njb3BlID0gdGhpcztcbiAgICB0aGlzLm1haW5fcHJvY2Vzc19waWQgPSB0aGlzLnNwYXduKGZ1bmN0aW9uKiAoKSB7XG4gICAgICB3aGlsZSAodHJ1ZSkge1xuICAgICAgICB5aWVsZCBwcm9jZXNzX3N5c3RlbV9zY29wZS5zbGVlcCgxMDAwMCk7XG4gICAgICB9XG4gICAgfSk7XG4gICAgdGhpcy5zZXRfY3VycmVudCh0aGlzLm1haW5fcHJvY2Vzc19waWQpO1xuICB9XG5cbiAgc3RhdGljICpydW4oZnVuLCBhcmdzLCBjb250ZXh0ID0gbnVsbCkge1xuICAgIGlmIChmdW4uY29uc3RydWN0b3IubmFtZSA9PT0gXCJHZW5lcmF0b3JGdW5jdGlvblwiKSB7XG4gICAgICByZXR1cm4geWllbGQqIGZ1bi5hcHBseShjb250ZXh0LCBhcmdzKTtcbiAgICB9IGVsc2Uge1xuICAgICAgcmV0dXJuIGZ1bi5hcHBseShjb250ZXh0LCBhcmdzKTtcbiAgICB9XG4gIH1cblxuICBzcGF3biguLi5hcmdzKSB7XG4gICAgaWYgKGFyZ3MubGVuZ3RoID09PSAxKSB7XG4gICAgICBsZXQgZnVuID0gYXJnc1swXTtcbiAgICAgIHJldHVybiB0aGlzLmFkZF9wcm9jKGZ1biwgW10sIGZhbHNlKS5waWQ7XG4gICAgfSBlbHNlIGlmIChhcmdzLmxlbmd0aCA9PT0gMykge1xuICAgICAgbGV0IG1vZCA9IGFyZ3NbMF07XG4gICAgICBsZXQgZnVuID0gYXJnc1sxXTtcbiAgICAgIGxldCB0aGVfYXJncyA9IGFyZ3NbMl07XG5cbiAgICAgIHJldHVybiB0aGlzLmFkZF9wcm9jKG1vZFtmdW5dLCB0aGVfYXJncywgZmFsc2UpLnBpZDtcbiAgICB9XG4gIH1cblxuICBzcGF3bl9saW5rKC4uLmFyZ3MpIHtcbiAgICBpZiAoYXJncy5sZW5ndGggPT09IDEpIHtcbiAgICAgIGxldCBmdW4gPSBhcmdzWzBdO1xuICAgICAgcmV0dXJuIHRoaXMuYWRkX3Byb2MoZnVuLCBbXSwgdHJ1ZSkucGlkO1xuICAgIH0gZWxzZSBpZiAoYXJncy5sZW5ndGggPT09IDMpIHtcbiAgICAgIGxldCBtb2QgPSBhcmdzWzBdO1xuICAgICAgbGV0IGZ1biA9IGFyZ3NbMV07XG4gICAgICBsZXQgdGhlX2FyZ3MgPSBhcmdzWzJdO1xuXG4gICAgICByZXR1cm4gdGhpcy5hZGRfcHJvYyhtb2RbZnVuXSwgdGhlX2FyZ3MsIHRydWUpLnBpZDtcbiAgICB9XG4gIH1cblxuICBsaW5rKHBpZCkge1xuICAgIHRoaXMubGlua3MuZ2V0KHRoaXMucGlkKCkpLmFkZChwaWQpO1xuICAgIHRoaXMubGlua3MuZ2V0KHBpZCkuYWRkKHRoaXMucGlkKCkpO1xuICB9XG5cbiAgdW5saW5rKHBpZCkge1xuICAgIHRoaXMubGlua3MuZ2V0KHRoaXMucGlkKCkpLmRlbGV0ZShwaWQpO1xuICAgIHRoaXMubGlua3MuZ2V0KHBpZCkuZGVsZXRlKHRoaXMucGlkKCkpO1xuICB9XG5cbiAgc2V0X2N1cnJlbnQoaWQpIHtcbiAgICBsZXQgcGlkID0gdGhpcy5waWRvZihpZCk7XG4gICAgaWYgKHBpZCAhPT0gbnVsbCkge1xuICAgICAgdGhpcy5jdXJyZW50X3Byb2Nlc3MgPSB0aGlzLnBpZHMuZ2V0KHBpZCk7XG4gICAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5zdGF0dXMgPSBTdGF0ZXMuUlVOTklORztcbiAgICB9XG4gIH1cblxuICBhZGRfcHJvYyhmdW4sIGFyZ3MsIGxpbmtlZCkge1xuICAgIGxldCBuZXdwaWQgPSBuZXcgUElEKCk7XG4gICAgbGV0IG1haWxib3ggPSBuZXcgTWFpbGJveCgpO1xuICAgIGxldCBuZXdwcm9jID0gbmV3IFByb2Nlc3MobmV3cGlkLCBmdW4sIGFyZ3MsIG1haWxib3gsIHRoaXMpO1xuXG4gICAgdGhpcy5waWRzLnNldChuZXdwaWQsIG5ld3Byb2MpO1xuICAgIHRoaXMubWFpbGJveGVzLnNldChuZXdwaWQsIG1haWxib3gpO1xuICAgIHRoaXMubGlua3Muc2V0KG5ld3BpZCwgbmV3IFNldCgpKTtcblxuICAgIGlmIChsaW5rZWQpIHtcbiAgICAgIHRoaXMubGluayhuZXdwaWQpO1xuICAgIH1cblxuICAgIG5ld3Byb2Muc3RhcnQoKTtcbiAgICByZXR1cm4gbmV3cHJvYztcbiAgfVxuXG4gIHJlbW92ZV9wcm9jKHBpZCwgZXhpdHJlYXNvbikge1xuICAgIHRoaXMucGlkcy5kZWxldGUocGlkKTtcbiAgICB0aGlzLnVucmVnaXN0ZXIocGlkKTtcbiAgICB0aGlzLnNjaGVkdWxlci5yZW1vdmVQaWQocGlkKTtcblxuICAgIGlmICh0aGlzLmxpbmtzLmhhcyhwaWQpKSB7XG4gICAgICBmb3IgKGxldCBsaW5rcGlkIG9mIHRoaXMubGlua3MuZ2V0KHBpZCkpIHtcbiAgICAgICAgdGhpcy5leGl0KGxpbmtwaWQsIGV4aXRyZWFzb24pO1xuICAgICAgICB0aGlzLmxpbmtzLmdldChsaW5rcGlkKS5kZWxldGUocGlkKTtcbiAgICAgIH1cblxuICAgICAgdGhpcy5saW5rcy5kZWxldGUocGlkKTtcbiAgICB9XG4gIH1cblxuICByZWdpc3RlcihuYW1lLCBwaWQpIHtcbiAgICBpZiAoIXRoaXMubmFtZXMuaGFzKG5hbWUpKSB7XG4gICAgICB0aGlzLm5hbWVzLnNldChuYW1lLCBwaWQpO1xuICAgIH0gZWxzZSB7XG4gICAgICB0aHJvdyBuZXcgRXJyb3IoXCJOYW1lIGlzIGFscmVhZHkgcmVnaXN0ZXJlZCB0byBhbm90aGVyIHByb2Nlc3NcIik7XG4gICAgfVxuICB9XG5cbiAgcmVnaXN0ZXJlZChuYW1lKSB7XG4gICAgcmV0dXJuIHRoaXMubmFtZXMuaGFzKG5hbWUpID8gdGhpcy5uYW1lcy5nZXQobmFtZSkgOiBudWxsO1xuICB9XG5cbiAgdW5yZWdpc3RlcihwaWQpIHtcbiAgICBmb3IgKGxldCBuYW1lIG9mIHRoaXMubmFtZXMua2V5cygpKSB7XG4gICAgICBpZiAodGhpcy5uYW1lcy5oYXMobmFtZSkgJiYgdGhpcy5uYW1lcy5nZXQobmFtZSkgPT09IHBpZCkge1xuICAgICAgICB0aGlzLm5hbWVzLmRlbGV0ZShuYW1lKTtcbiAgICAgIH1cbiAgICB9XG4gIH1cblxuICBwaWQoKSB7XG4gICAgcmV0dXJuIHRoaXMuY3VycmVudF9wcm9jZXNzLnBpZDtcbiAgfVxuXG4gIHBpZG9mKGlkKSB7XG4gICAgaWYgKGlkIGluc3RhbmNlb2YgUElEKSB7XG4gICAgICByZXR1cm4gdGhpcy5waWRzLmhhcyhpZCkgPyBpZCA6IG51bGw7XG4gICAgfSBlbHNlIGlmIChpZCBpbnN0YW5jZW9mIFByb2Nlc3MpIHtcbiAgICAgIHJldHVybiBpZC5waWQ7XG4gICAgfSBlbHNlIHtcbiAgICAgIGxldCBwaWQgPSB0aGlzLnJlZ2lzdGVyZWQoaWQpO1xuICAgICAgaWYgKHBpZCA9PT0gbnVsbCkgdGhyb3cgXCJQcm9jZXNzIG5hbWUgbm90IHJlZ2lzdGVyZWQ6IFwiICsgaWQgKyBcIiAoXCIgKyB0eXBlb2YgaWQgKyBcIilcIjtcbiAgICAgIHJldHVybiBwaWQ7XG4gICAgfVxuICB9XG5cbiAgc2VuZChpZCwgbXNnKSB7XG4gICAgY29uc3QgcGlkID0gdGhpcy5waWRvZihpZCk7XG5cbiAgICBpZiAocGlkKSB7XG4gICAgICB0aGlzLm1haWxib3hlcy5nZXQocGlkKS5kZWxpdmVyKG1zZyk7XG5cbiAgICAgIGlmICh0aGlzLnN1c3BlbmRlZC5oYXMocGlkKSkge1xuICAgICAgICBsZXQgZnVuID0gdGhpcy5zdXNwZW5kZWQuZ2V0KHBpZCk7XG4gICAgICAgIHRoaXMuc3VzcGVuZGVkLmRlbGV0ZShwaWQpO1xuICAgICAgICB0aGlzLnNjaGVkdWxlKGZ1bik7XG4gICAgICB9XG4gICAgfVxuXG4gICAgcmV0dXJuIG1zZztcbiAgfVxuXG4gIHJlY2VpdmUoZnVuLCB0aW1lb3V0ID0gMCwgdGltZW91dEZuID0gKCkgPT4gdHJ1ZSkge1xuICAgIGxldCBEYXRlVGltZW91dCA9IG51bGw7XG5cbiAgICBpZiAodGltZW91dCA9PT0gMCB8fCB0aW1lb3V0ID09PSBJbmZpbml0eSkge1xuICAgICAgRGF0ZVRpbWVvdXQgPSBudWxsO1xuICAgIH0gZWxzZSB7XG4gICAgICBEYXRlVGltZW91dCA9IERhdGUubm93KCkgKyB0aW1lb3V0O1xuICAgIH1cblxuICAgIHJldHVybiBbU3RhdGVzLlJFQ0VJVkUsIGZ1biwgRGF0ZVRpbWVvdXQsIHRpbWVvdXRGbl07XG4gIH1cblxuICBzbGVlcChkdXJhdGlvbikge1xuICAgIHJldHVybiBbU3RhdGVzLlNMRUVQLCBkdXJhdGlvbl07XG4gIH1cblxuICBzdXNwZW5kKGZ1bikge1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLnN0YXR1cyA9IFN0YXRlcy5TVVNQRU5ERUQ7XG4gICAgdGhpcy5zdXNwZW5kZWQuc2V0KHRoaXMuY3VycmVudF9wcm9jZXNzLnBpZCwgZnVuKTtcbiAgfVxuXG4gIGRlbGF5KGZ1biwgdGltZSkge1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLnN0YXR1cyA9IFN0YXRlcy5TTEVFUElORztcbiAgICB0aGlzLnNjaGVkdWxlci5zY2hlZHVsZUZ1dHVyZSh0aGlzLmN1cnJlbnRfcHJvY2Vzcy5waWQsIHRpbWUsIGZ1bik7XG4gIH1cblxuICBzY2hlZHVsZShmdW4sIHBpZCkge1xuICAgIGNvbnN0IHRoZV9waWQgPSBwaWQgIT0gbnVsbCA/IHBpZCA6IHRoaXMuY3VycmVudF9wcm9jZXNzLnBpZDtcbiAgICB0aGlzLnNjaGVkdWxlci5zY2hlZHVsZSh0aGVfcGlkLCBmdW4pO1xuICB9XG5cbiAgZXhpdChvbmUsIHR3bykge1xuICAgIGlmICh0d28pIHtcbiAgICAgIGxldCBwaWQgPSBvbmU7XG4gICAgICBsZXQgcmVhc29uID0gdHdvO1xuXG4gICAgICBsZXQgcHJvY2VzcyA9IHRoaXMucGlkcy5nZXQodGhpcy5waWRvZihwaWQpKTtcbiAgICAgIGlmIChwcm9jZXNzICYmIHByb2Nlc3MuaXNfdHJhcHBpbmdfZXhpdHMoKSB8fCByZWFzb24gPT09IFN0YXRlcy5LSUxMIHx8IHJlYXNvbiA9PT0gU3RhdGVzLk5PUk1BTCkge1xuICAgICAgICB0aGlzLm1haWxib3hlcy5nZXQocHJvY2Vzcy5waWQpLmRlbGl2ZXIoW1N0YXRlcy5FWElULCB0aGlzLnBpZCgpLCByZWFzb25dKTtcbiAgICAgIH0gZWxzZSB7XG4gICAgICAgIHByb2Nlc3Muc2lnbmFsKHJlYXNvbik7XG4gICAgICB9XG4gICAgfSBlbHNlIHtcbiAgICAgIGxldCByZWFzb24gPSBvbmU7XG4gICAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5zaWduYWwocmVhc29uKTtcbiAgICB9XG4gIH1cblxuICBlcnJvcihyZWFzb24pIHtcbiAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5zaWduYWwocmVhc29uKTtcbiAgfVxuXG4gIHByb2Nlc3NfZmxhZyhmbGFnLCB2YWx1ZSkge1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLnByb2Nlc3NfZmxhZyhmbGFnLCB2YWx1ZSk7XG4gIH1cblxuICBwdXQoa2V5LCB2YWx1ZSkge1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3Rba2V5XSA9IHZhbHVlO1xuICB9XG5cbiAgZ2V0KGtleSkge1xuICAgIGlmIChrZXkgIT0gbnVsbCkge1xuICAgICAgcmV0dXJuIHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3Rba2V5XTtcbiAgICB9IGVsc2Uge1xuICAgICAgcmV0dXJuIHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3Q7XG4gICAgfVxuICB9XG5cbiAgZ2V0X2tleXMoKSB7XG4gICAgcmV0dXJuIE9iamVjdC5rZXlzKHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3QpO1xuICB9XG5cbiAgZXJhc2Uoa2V5KSB7XG4gICAgaWYgKGtleSAhPSBudWxsKSB7XG4gICAgICBkZWxldGUgdGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdFtrZXldO1xuICAgIH0gZWxzZSB7XG4gICAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0ID0ge307XG4gICAgfVxuICB9XG59XG5cbmZ1bmN0aW9uIHN0YXJ0KG1vZHVsZSwgYXJncykge1xuICByZXR1cm4gW1N5bWJvbC5mb3IoXCJva1wiKSwgc2VsZi5zeXN0ZW0uc3Bhd24oc3RhcnRfcHJvY2Vzcyhtb2R1bGUsIGFyZ3MpKV07XG59XG5cbmZ1bmN0aW9uIHN0YXJ0X2xpbmsobW9kdWxlLCBhcmdzKSB7XG4gIHJldHVybiBbU3ltYm9sLmZvcihcIm9rXCIpLCBzZWxmLnN5c3RlbS5zcGF3bl9saW5rKHN0YXJ0X3Byb2Nlc3MobW9kdWxlLCBhcmdzKSldO1xufVxuXG5mdW5jdGlvbiBzdGFydF9wcm9jZXNzKG1vZHVsZSwgYXJncykge1xuICByZXR1cm4gZnVuY3Rpb24qICgpIHtcbiAgICBsZXQgW29rLCBzdGF0ZV0gPSBtb2R1bGUuaW5pdC5hcHBseShudWxsLCBbYXJnc10pO1xuICAgIHlpZWxkIHNlbGYuc3lzdGVtLnB1dChcInN0YXRlXCIsIHN0YXRlKTtcblxuICAgIHRyeSB7XG4gICAgICB3aGlsZSAodHJ1ZSkge1xuICAgICAgICB5aWVsZCBzZWxmLnN5c3RlbS5yZWNlaXZlKGZ1bmN0aW9uIChhcmdzKSB7XG4gICAgICAgICAgbGV0IGNvbW1hbmQgPSBhcmdzWzBdO1xuXG4gICAgICAgICAgc3dpdGNoIChjb21tYW5kKSB7XG4gICAgICAgICAgICBjYXNlIFwiY2FsbFwiOlxuICAgICAgICAgICAgICB2YXIgcmVxdWVzdCA9IGFyZ3NbMV07XG4gICAgICAgICAgICAgIHZhciBzZW5kZXIgPSBhcmdzWzJdO1xuXG4gICAgICAgICAgICAgIHZhciBbcmVwbHksIHJlc3BvbnNlLCBuZXdfc3RhdGVdID0gbW9kdWxlLmhhbmRsZV9jYWxsKHJlcXVlc3QsIHNlbmRlciwgc2VsZi5zeXN0ZW0uZ2V0KFwic3RhdGVcIikpO1xuICAgICAgICAgICAgICBzZWxmLnN5c3RlbS5wdXQoXCJzdGF0ZVwiLCBuZXdfc3RhdGUpO1xuXG4gICAgICAgICAgICAgIHNlbGYuc3lzdGVtLnNlbmQoc2VuZGVyLCByZXNwb25zZSk7XG4gICAgICAgICAgICAgIGJyZWFrO1xuXG4gICAgICAgICAgICBjYXNlIFwiY2FzdFwiOlxuICAgICAgICAgICAgICB2YXIgcmVxdWVzdCA9IGFyZ3NbMV07XG4gICAgICAgICAgICAgIHZhciBzZW5kZXIgPSBhcmdzWzJdO1xuXG4gICAgICAgICAgICAgIHZhciBbcmVwbHksIG5ld19zdGF0ZV0gPSBtb2R1bGUuaGFuZGxlX2Nhc3QocmVxdWVzdCwgc2VsZi5zeXN0ZW0uZ2V0KFwic3RhdGVcIikpO1xuXG4gICAgICAgICAgICAgIHNlbGYuc3lzdGVtLnB1dChcInN0YXRlXCIsIG5ld19zdGF0ZSk7XG4gICAgICAgICAgICAgIHNlbGYuc3lzdGVtLnNlbmQoYXJnc1syXSwgU3ltYm9sLmZvcihcIm9rXCIpKTtcblxuICAgICAgICAgICAgICBicmVhaztcblxuICAgICAgICAgICAgY2FzZSBcInN0b3BcIjpcbiAgICAgICAgICAgICAgdGhyb3cgXCJzdG9wXCI7XG4gICAgICAgICAgfVxuICAgICAgICB9KTtcbiAgICAgIH1cbiAgICB9IGNhdGNoIChlKSB7XG4gICAgICBpZiAoZSAhPT0gXCJzdG9wXCIpIHtcbiAgICAgICAgdGhyb3cgZTtcbiAgICAgIH1cbiAgICB9XG4gIH07XG59XG5cbmZ1bmN0aW9uKiBjYWxsKHNlcnZlciwgcmVxdWVzdCkge1xuICBzZWxmLnN5c3RlbS5zZW5kKHNlcnZlciwgW1wiY2FsbFwiLCByZXF1ZXN0LCBzZWxmLnN5c3RlbS5waWQoKV0pO1xuXG4gIHJldHVybiB5aWVsZCBzZWxmLnN5c3RlbS5yZWNlaXZlKGZ1bmN0aW9uIChhcmdzKSB7XG4gICAgcmV0dXJuIGFyZ3M7XG4gIH0pO1xufVxuXG5mdW5jdGlvbiogY2FzdChzZXJ2ZXIsIHJlcXVlc3QpIHtcbiAgc2VsZi5zeXN0ZW0uc2VuZChzZXJ2ZXIsIFtcImNhc3RcIiwgcmVxdWVzdCwgc2VsZi5zeXN0ZW0ucGlkKCldKTtcblxuICByZXR1cm4geWllbGQgc2VsZi5zeXN0ZW0ucmVjZWl2ZShmdW5jdGlvbiAoYXJncykge1xuICAgIHJldHVybiBhcmdzO1xuICB9KTtcbn1cblxuZnVuY3Rpb24gc3RvcChzZXJ2ZXIpIHtcbiAgc2VsZi5zeXN0ZW0uc2VuZChzZXJ2ZXIsIFtcInN0b3BcIl0pO1xufVxuXG52YXIgZ2VuX3NlcnZlciA9IHsgc3RhcnQsIHN0YXJ0X2xpbmssIGNhbGwsIGNhc3QsIHN0b3AgfTtcblxuZXhwb3J0IHsgUHJvY2Vzc1N5c3RlbSwgZ2VuX3NlcnZlciBhcyBHZW5TZXJ2ZXIgfTsiXSwic291cmNlUm9vdCI6Ii9zb3VyY2UvIn0=
