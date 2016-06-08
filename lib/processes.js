"use strict";

var _createClass = (function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; })();

var _slicedToArray = (function () { function sliceIterator(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"]) _i["return"](); } finally { if (_d) throw _e; } } return _arr; } return function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { return sliceIterator(arr, i); } else { throw new TypeError("Invalid attempt to destructure non-iterable instance"); } }; })();

Object.defineProperty(exports, "__esModule", {
  value: true
});

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

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

var GenServer = { start: start, start_link: start_link, call: call, cast: cast, stop: stop };

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

var index = {
  ProcessSystem: ProcessSystem,
  GenServer: GenServer
};

exports.default = index;
//# sourceMappingURL=data:application/json;base64,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RUFBRSxJQUFJLENBQUMsQ0FBQztPQUNqQztLQUNGOzs7U0E1QkcsYUFBYTs7O0FBK09uQixJQUFJLEtBQUssR0FBRztBQUNWLGVBQWEsRUFBYixhQUFhO0FBQ2IsV0FBUyxFQUFULFNBQVM7Q0FDVixDQUFDOztrQkFFYSxLQUFLIiwiZmlsZSI6ImluZGV4LmpzIiwic291cmNlc0NvbnRlbnQiOlsiZnVuY3Rpb24gc3RhcnQobW9kdWxlLCBhcmdzKSB7XG4gIHJldHVybiBbU3ltYm9sLmZvcihcIm9rXCIpLCBzZWxmLnN5c3RlbS5zcGF3bihzdGFydF9wcm9jZXNzKG1vZHVsZSwgYXJncykpXTtcbn1cblxuZnVuY3Rpb24gc3RhcnRfbGluayhtb2R1bGUsIGFyZ3MpIHtcbiAgcmV0dXJuIFtTeW1ib2wuZm9yKFwib2tcIiksIHNlbGYuc3lzdGVtLnNwYXduX2xpbmsoc3RhcnRfcHJvY2Vzcyhtb2R1bGUsIGFyZ3MpKV07XG59XG5cbmZ1bmN0aW9uIHN0YXJ0X3Byb2Nlc3MobW9kdWxlLCBhcmdzKSB7XG4gIHJldHVybiBmdW5jdGlvbiogKCkge1xuICAgIGxldCBbb2ssIHN0YXRlXSA9IG1vZHVsZS5pbml0LmFwcGx5KG51bGwsIFthcmdzXSk7XG4gICAgeWllbGQgc2VsZi5zeXN0ZW0ucHV0KFwic3RhdGVcIiwgc3RhdGUpO1xuXG4gICAgdHJ5IHtcbiAgICAgIHdoaWxlICh0cnVlKSB7XG4gICAgICAgIHlpZWxkIHNlbGYuc3lzdGVtLnJlY2VpdmUoZnVuY3Rpb24gKGFyZ3MpIHtcbiAgICAgICAgICBsZXQgY29tbWFuZCA9IGFyZ3NbMF07XG5cbiAgICAgICAgICBzd2l0Y2ggKGNvbW1hbmQpIHtcbiAgICAgICAgICAgIGNhc2UgXCJjYWxsXCI6XG4gICAgICAgICAgICAgIHZhciByZXF1ZXN0ID0gYXJnc1sxXTtcbiAgICAgICAgICAgICAgdmFyIHNlbmRlciA9IGFyZ3NbMl07XG5cbiAgICAgICAgICAgICAgdmFyIFtyZXBseSwgcmVzcG9uc2UsIG5ld19zdGF0ZV0gPSBtb2R1bGUuaGFuZGxlX2NhbGwocmVxdWVzdCwgc2VuZGVyLCBzZWxmLnN5c3RlbS5nZXQoXCJzdGF0ZVwiKSk7XG4gICAgICAgICAgICAgIHNlbGYuc3lzdGVtLnB1dChcInN0YXRlXCIsIG5ld19zdGF0ZSk7XG5cbiAgICAgICAgICAgICAgc2VsZi5zeXN0ZW0uc2VuZChzZW5kZXIsIHJlc3BvbnNlKTtcbiAgICAgICAgICAgICAgYnJlYWs7XG5cbiAgICAgICAgICAgIGNhc2UgXCJjYXN0XCI6XG4gICAgICAgICAgICAgIHZhciByZXF1ZXN0ID0gYXJnc1sxXTtcbiAgICAgICAgICAgICAgdmFyIHNlbmRlciA9IGFyZ3NbMl07XG5cbiAgICAgICAgICAgICAgdmFyIFtyZXBseSwgbmV3X3N0YXRlXSA9IG1vZHVsZS5oYW5kbGVfY2FzdChyZXF1ZXN0LCBzZWxmLnN5c3RlbS5nZXQoXCJzdGF0ZVwiKSk7XG5cbiAgICAgICAgICAgICAgc2VsZi5zeXN0ZW0ucHV0KFwic3RhdGVcIiwgbmV3X3N0YXRlKTtcbiAgICAgICAgICAgICAgc2VsZi5zeXN0ZW0uc2VuZChhcmdzWzJdLCBTeW1ib2wuZm9yKFwib2tcIikpO1xuXG4gICAgICAgICAgICAgIGJyZWFrO1xuXG4gICAgICAgICAgICBjYXNlIFwic3RvcFwiOlxuICAgICAgICAgICAgICB0aHJvdyBcInN0b3BcIjtcbiAgICAgICAgICB9XG4gICAgICAgIH0pO1xuICAgICAgfVxuICAgIH0gY2F0Y2ggKGUpIHtcbiAgICAgIGlmIChlICE9PSBcInN0b3BcIikge1xuICAgICAgICB0aHJvdyBlO1xuICAgICAgfVxuICAgIH1cbiAgfTtcbn1cblxuZnVuY3Rpb24qIGNhbGwoc2VydmVyLCByZXF1ZXN0KSB7XG4gIHNlbGYuc3lzdGVtLnNlbmQoc2VydmVyLCBbXCJjYWxsXCIsIHJlcXVlc3QsIHNlbGYuc3lzdGVtLnBpZCgpXSk7XG5cbiAgcmV0dXJuIHlpZWxkIHNlbGYuc3lzdGVtLnJlY2VpdmUoZnVuY3Rpb24gKGFyZ3MpIHtcbiAgICByZXR1cm4gYXJncztcbiAgfSk7XG59XG5cbmZ1bmN0aW9uKiBjYXN0KHNlcnZlciwgcmVxdWVzdCkge1xuICBzZWxmLnN5c3RlbS5zZW5kKHNlcnZlciwgW1wiY2FzdFwiLCByZXF1ZXN0LCBzZWxmLnN5c3RlbS5waWQoKV0pO1xuXG4gIHJldHVybiB5aWVsZCBzZWxmLnN5c3RlbS5yZWNlaXZlKGZ1bmN0aW9uIChhcmdzKSB7XG4gICAgcmV0dXJuIGFyZ3M7XG4gIH0pO1xufVxuXG5mdW5jdGlvbiBzdG9wKHNlcnZlcikge1xuICBzZWxmLnN5c3RlbS5zZW5kKHNlcnZlciwgW1wic3RvcFwiXSk7XG59XG5cbnZhciBHZW5TZXJ2ZXIgPSB7IHN0YXJ0LCBzdGFydF9saW5rLCBjYWxsLCBjYXN0LCBzdG9wIH07XG5cbmNsYXNzIE1haWxib3gge1xuXG4gIGNvbnN0cnVjdG9yKCkge1xuICAgIHRoaXMubWVzc2FnZXMgPSBbXTtcbiAgfVxuXG4gIGRlbGl2ZXIobWVzc2FnZSkge1xuICAgIHRoaXMubWVzc2FnZXMucHVzaChtZXNzYWdlKTtcbiAgICByZXR1cm4gbWVzc2FnZTtcbiAgfVxuXG4gIGdldCgpIHtcbiAgICByZXR1cm4gdGhpcy5tZXNzYWdlcztcbiAgfVxuXG4gIGlzRW1wdHkoKSB7XG4gICAgcmV0dXJuIHRoaXMubWVzc2FnZXMubGVuZ3RoID09PSAwO1xuICB9XG5cbiAgcmVtb3ZlQXQoaW5kZXgpIHtcbiAgICB0aGlzLm1lc3NhZ2VzLnNwbGljZShpbmRleCwgMSk7XG4gIH1cbn1cblxudmFyIFN0YXRlcyA9IHtcbiAgTk9STUFMOiBTeW1ib2wuZm9yKFwibm9ybWFsXCIpLFxuICBLSUxMOiBTeW1ib2wuZm9yKFwia2lsbFwiKSxcbiAgU1VTUEVORDogU3ltYm9sLmZvcihcInN1c3BlbmRcIiksXG4gIENPTlRJTlVFOiBTeW1ib2wuZm9yKFwiY29udGludWVcIiksXG4gIFJFQ0VJVkU6IFN5bWJvbC5mb3IoXCJyZWNlaXZlXCIpLFxuICBTRU5EOiBTeW1ib2wuZm9yKFwic2VuZFwiKSxcbiAgU0xFRVBJTkc6IFN5bWJvbC5mb3IoXCJzbGVlcGluZ1wiKSxcbiAgUlVOTklORzogU3ltYm9sLmZvcihcInJ1bm5pbmdcIiksXG4gIFNVU1BFTkRFRDogU3ltYm9sLmZvcihcInN1c3BlbmRlZFwiKSxcbiAgU1RPUFBFRDogU3ltYm9sLmZvcihcInN0b3BwZWRcIiksXG4gIFNMRUVQOiBTeW1ib2wuZm9yKFwic2xlZXBcIiksXG4gIEVYSVQ6IFN5bWJvbC5mb3IoXCJleGl0XCIpLFxuICBOT01BVENIOiBTeW1ib2wuZm9yKFwibm9fbWF0Y2hcIilcbn07XG5cbmZ1bmN0aW9uIGlzX3NsZWVwKHZhbHVlKSB7XG4gIHJldHVybiBBcnJheS5pc0FycmF5KHZhbHVlKSAmJiB2YWx1ZVswXSA9PT0gU3RhdGVzLlNMRUVQO1xufVxuXG5mdW5jdGlvbiBpc19yZWNlaXZlKHZhbHVlKSB7XG4gIHJldHVybiBBcnJheS5pc0FycmF5KHZhbHVlKSAmJiB2YWx1ZVswXSA9PT0gU3RhdGVzLlJFQ0VJVkU7XG59XG5cbmZ1bmN0aW9uIHJlY2VpdmVfdGltZWRfb3V0KHZhbHVlKSB7XG4gIHJldHVybiB2YWx1ZVsyXSAhPSBudWxsICYmIHZhbHVlWzJdIDwgRGF0ZS5ub3coKTtcbn1cblxuY2xhc3MgUHJvY2VzcyB7XG5cbiAgY29uc3RydWN0b3IocGlkLCBmdW5jLCBhcmdzLCBtYWlsYm94LCBzeXN0ZW0pIHtcbiAgICB0aGlzLnBpZCA9IHBpZDtcbiAgICB0aGlzLmZ1bmMgPSBmdW5jO1xuICAgIHRoaXMuYXJncyA9IGFyZ3M7XG4gICAgdGhpcy5tYWlsYm94ID0gbWFpbGJveDtcbiAgICB0aGlzLnN5c3RlbSA9IHN5c3RlbTtcbiAgICB0aGlzLnN0YXR1cyA9IFN0YXRlcy5TVE9QUEVEO1xuICAgIHRoaXMuZGljdCA9IHt9O1xuICAgIHRoaXMuZmxhZ3MgPSB7fTtcbiAgfVxuXG4gIHN0YXJ0KCkge1xuICAgIGNvbnN0IGZ1bmN0aW9uX3Njb3BlID0gdGhpcztcbiAgICBsZXQgbWFjaGluZSA9IHRoaXMubWFpbigpO1xuXG4gICAgdGhpcy5zeXN0ZW0uc2NoZWR1bGUoZnVuY3Rpb24gKCkge1xuICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgbWFjaGluZS5uZXh0KCkpO1xuICAgIH0sIHRoaXMucGlkKTtcbiAgfVxuXG4gICptYWluKCkge1xuICAgIGxldCByZXR2YWwgPSBTdGF0ZXMuTk9STUFMO1xuXG4gICAgdHJ5IHtcbiAgICAgIHlpZWxkKiB0aGlzLmZ1bmMuYXBwbHkobnVsbCwgdGhpcy5hcmdzKTtcbiAgICB9IGNhdGNoIChlKSB7XG4gICAgICBjb25zb2xlLmVycm9yKGUpO1xuICAgICAgcmV0dmFsID0gZTtcbiAgICB9XG5cbiAgICB0aGlzLnN5c3RlbS5leGl0KHJldHZhbCk7XG4gIH1cblxuICBwcm9jZXNzX2ZsYWcoZmxhZywgdmFsdWUpIHtcbiAgICB0aGlzLmZsYWdzW2ZsYWddID0gdmFsdWU7XG4gIH1cblxuICBpc190cmFwcGluZ19leGl0cygpIHtcbiAgICByZXR1cm4gdGhpcy5mbGFnc1tTeW1ib2wuZm9yKFwidHJhcF9leGl0XCIpXSAmJiB0aGlzLmZsYWdzW1N5bWJvbC5mb3IoXCJ0cmFwX2V4aXRcIildID09IHRydWU7XG4gIH1cblxuICBzaWduYWwocmVhc29uKSB7XG4gICAgaWYgKHJlYXNvbiAhPT0gU3RhdGVzLk5PUk1BTCkge1xuICAgICAgY29uc29sZS5lcnJvcihyZWFzb24pO1xuICAgIH1cblxuICAgIHRoaXMuc3lzdGVtLnJlbW92ZV9wcm9jKHRoaXMucGlkLCByZWFzb24pO1xuICB9XG5cbiAgcmVjZWl2ZShmdW4pIHtcbiAgICBsZXQgdmFsdWUgPSBTdGF0ZXMuTk9NQVRDSDtcbiAgICBsZXQgbWVzc2FnZXMgPSB0aGlzLm1haWxib3guZ2V0KCk7XG5cbiAgICBmb3IgKGxldCBpID0gMDsgaSA8IG1lc3NhZ2VzLmxlbmd0aDsgaSsrKSB7XG4gICAgICB0cnkge1xuICAgICAgICB2YWx1ZSA9IGZ1bihtZXNzYWdlc1tpXSk7XG4gICAgICAgIGlmICh2YWx1ZSAhPT0gU3RhdGVzLk5PTUFUQ0gpIHtcbiAgICAgICAgICB0aGlzLm1haWxib3gucmVtb3ZlQXQoaSk7XG4gICAgICAgICAgYnJlYWs7XG4gICAgICAgIH1cbiAgICAgIH0gY2F0Y2ggKGUpIHtcbiAgICAgICAgdGhpcy5leGl0KGUpO1xuICAgICAgfVxuICAgIH1cblxuICAgIHJldHVybiB2YWx1ZTtcbiAgfVxuXG4gIHJ1bihtYWNoaW5lLCBzdGVwKSB7XG4gICAgY29uc3QgZnVuY3Rpb25fc2NvcGUgPSB0aGlzO1xuXG4gICAgaWYgKCFzdGVwLmRvbmUpIHtcbiAgICAgIGxldCB2YWx1ZSA9IHN0ZXAudmFsdWU7XG5cbiAgICAgIGlmIChpc19zbGVlcCh2YWx1ZSkpIHtcblxuICAgICAgICB0aGlzLnN5c3RlbS5kZWxheShmdW5jdGlvbiAoKSB7XG4gICAgICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICAgICAgZnVuY3Rpb25fc2NvcGUucnVuKG1hY2hpbmUsIG1hY2hpbmUubmV4dCgpKTtcbiAgICAgICAgfSwgdmFsdWVbMV0pO1xuICAgICAgfSBlbHNlIGlmIChpc19yZWNlaXZlKHZhbHVlKSAmJiByZWNlaXZlX3RpbWVkX291dCh2YWx1ZSkpIHtcblxuICAgICAgICBsZXQgcmVzdWx0ID0gdmFsdWVbM10oKTtcblxuICAgICAgICB0aGlzLnN5c3RlbS5zY2hlZHVsZShmdW5jdGlvbiAoKSB7XG4gICAgICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICAgICAgZnVuY3Rpb25fc2NvcGUucnVuKG1hY2hpbmUsIG1hY2hpbmUubmV4dChyZXN1bHQpKTtcbiAgICAgICAgfSk7XG4gICAgICB9IGVsc2UgaWYgKGlzX3JlY2VpdmUodmFsdWUpKSB7XG5cbiAgICAgICAgbGV0IHJlc3VsdCA9IGZ1bmN0aW9uX3Njb3BlLnJlY2VpdmUodmFsdWVbMV0pO1xuXG4gICAgICAgIGlmIChyZXN1bHQgPT09IFN0YXRlcy5OT01BVENIKSB7XG4gICAgICAgICAgdGhpcy5zeXN0ZW0uc3VzcGVuZChmdW5jdGlvbiAoKSB7XG4gICAgICAgICAgICBmdW5jdGlvbl9zY29wZS5zeXN0ZW0uc2V0X2N1cnJlbnQoZnVuY3Rpb25fc2NvcGUucGlkKTtcbiAgICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnJ1bihtYWNoaW5lLCBzdGVwKTtcbiAgICAgICAgICB9KTtcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICB0aGlzLnN5c3RlbS5zY2hlZHVsZShmdW5jdGlvbiAoKSB7XG4gICAgICAgICAgICBmdW5jdGlvbl9zY29wZS5zeXN0ZW0uc2V0X2N1cnJlbnQoZnVuY3Rpb25fc2NvcGUucGlkKTtcbiAgICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnJ1bihtYWNoaW5lLCBtYWNoaW5lLm5leHQocmVzdWx0KSk7XG4gICAgICAgICAgfSk7XG4gICAgICAgIH1cbiAgICAgIH0gZWxzZSB7XG4gICAgICAgIHRoaXMuc3lzdGVtLnNjaGVkdWxlKGZ1bmN0aW9uICgpIHtcbiAgICAgICAgICBmdW5jdGlvbl9zY29wZS5zeXN0ZW0uc2V0X2N1cnJlbnQoZnVuY3Rpb25fc2NvcGUucGlkKTtcbiAgICAgICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgbWFjaGluZS5uZXh0KHZhbHVlKSk7XG4gICAgICAgIH0pO1xuICAgICAgfVxuICAgIH1cbiAgfVxufVxuXG5jbGFzcyBQcm9jZXNzUXVldWUge1xuICBjb25zdHJ1Y3RvcihwaWQpIHtcbiAgICB0aGlzLnBpZCA9IHBpZDtcbiAgICB0aGlzLnRhc2tzID0gW107XG4gIH1cblxuICBlbXB0eSgpIHtcbiAgICByZXR1cm4gdGhpcy50YXNrcy5sZW5ndGggPT09IDA7XG4gIH1cblxuICBhZGQodGFzaykge1xuICAgIHRoaXMudGFza3MucHVzaCh0YXNrKTtcbiAgfVxuXG4gIG5leHQoKSB7XG4gICAgcmV0dXJuIHRoaXMudGFza3Muc2hpZnQoKTtcbiAgfVxufVxuXG5jbGFzcyBTY2hlZHVsZXIge1xuICBjb25zdHJ1Y3Rvcih0aHJvdHRsZSA9IDAsIHJlZHVjdGlvbnNfcGVyX3Byb2Nlc3MgPSA4KSB7XG4gICAgdGhpcy5pc1J1bm5pbmcgPSBmYWxzZTtcbiAgICB0aGlzLmludm9rZUxhdGVyID0gZnVuY3Rpb24gKGNhbGxiYWNrKSB7XG4gICAgICBzZXRUaW1lb3V0KGNhbGxiYWNrLCB0aHJvdHRsZSk7XG4gICAgfTtcblxuICAgIC8vIEluIG91ciBjYXNlIGEgcmVkdWN0aW9uIGlzIGVxdWFsIHRvIGEgdGFzayBjYWxsXG4gICAgLy8gQ29udHJvbHMgaG93IG1hbnkgdGFza3MgYXJlIGNhbGxlZCBhdCBhIHRpbWUgcGVyIHByb2Nlc3NcbiAgICB0aGlzLnJlZHVjdGlvbnNfcGVyX3Byb2Nlc3MgPSByZWR1Y3Rpb25zX3Blcl9wcm9jZXNzO1xuICAgIHRoaXMucXVldWVzID0gbmV3IE1hcCgpO1xuICAgIHRoaXMucnVuKCk7XG4gIH1cblxuICBhZGRUb1F1ZXVlKHBpZCwgdGFzaykge1xuICAgIGlmICghdGhpcy5xdWV1ZXMuaGFzKHBpZCkpIHtcbiAgICAgIHRoaXMucXVldWVzLnNldChwaWQsIG5ldyBQcm9jZXNzUXVldWUocGlkKSk7XG4gICAgfVxuXG4gICAgdGhpcy5xdWV1ZXMuZ2V0KHBpZCkuYWRkKHRhc2spO1xuICB9XG5cbiAgcmVtb3ZlUGlkKHBpZCkge1xuICAgIHRoaXMuaXNSdW5uaW5nID0gdHJ1ZTtcblxuICAgIHRoaXMucXVldWVzLmRlbGV0ZShwaWQpO1xuXG4gICAgdGhpcy5pc1J1bm5pbmcgPSBmYWxzZTtcbiAgfVxuXG4gIHJ1bigpIHtcbiAgICBpZiAodGhpcy5pc1J1bm5pbmcpIHtcbiAgICAgIHRoaXMuaW52b2tlTGF0ZXIoKCkgPT4ge1xuICAgICAgICB0aGlzLnJ1bigpO1xuICAgICAgfSk7XG4gICAgfSBlbHNlIHtcbiAgICAgIGZvciAobGV0IFtwaWQsIHF1ZXVlXSBvZiB0aGlzLnF1ZXVlcykge1xuICAgICAgICBsZXQgcmVkdWN0aW9ucyA9IDA7XG4gICAgICAgIHdoaWxlIChxdWV1ZSAmJiAhcXVldWUuZW1wdHkoKSAmJiByZWR1Y3Rpb25zIDwgdGhpcy5yZWR1Y3Rpb25zX3Blcl9wcm9jZXNzKSB7XG4gICAgICAgICAgbGV0IHRhc2sgPSBxdWV1ZS5uZXh0KCk7XG4gICAgICAgICAgdGhpcy5pc1J1bm5pbmcgPSB0cnVlO1xuXG4gICAgICAgICAgbGV0IHJlc3VsdDtcblxuICAgICAgICAgIHRyeSB7XG4gICAgICAgICAgICByZXN1bHQgPSB0YXNrKCk7XG4gICAgICAgICAgfSBjYXRjaCAoZSkge1xuICAgICAgICAgICAgY29uc29sZS5lcnJvcihlKTtcbiAgICAgICAgICAgIHJlc3VsdCA9IGU7XG4gICAgICAgICAgfVxuXG4gICAgICAgICAgdGhpcy5pc1J1bm5pbmcgPSBmYWxzZTtcblxuICAgICAgICAgIGlmIChyZXN1bHQgaW5zdGFuY2VvZiBFcnJvcikge1xuICAgICAgICAgICAgdGhyb3cgcmVzdWx0O1xuICAgICAgICAgIH1cblxuICAgICAgICAgIHJlZHVjdGlvbnMrKztcbiAgICAgICAgfVxuICAgICAgfVxuXG4gICAgICB0aGlzLmludm9rZUxhdGVyKCgpID0+IHtcbiAgICAgICAgdGhpcy5ydW4oKTtcbiAgICAgIH0pO1xuICAgIH1cbiAgfVxuXG4gIGFkZFRvU2NoZWR1bGVyKHBpZCwgdGFzaywgZHVlVGltZSA9IDApIHtcbiAgICBpZiAoZHVlVGltZSA9PT0gMCkge1xuICAgICAgdGhpcy5pbnZva2VMYXRlcigoKSA9PiB7XG4gICAgICAgIHRoaXMuYWRkVG9RdWV1ZShwaWQsIHRhc2spO1xuICAgICAgfSk7XG4gICAgfSBlbHNlIHtcbiAgICAgIHNldFRpbWVvdXQoKCkgPT4ge1xuICAgICAgICB0aGlzLmFkZFRvUXVldWUocGlkLCB0YXNrKTtcbiAgICAgIH0sIGR1ZVRpbWUpO1xuICAgIH1cbiAgfVxuXG4gIHNjaGVkdWxlKHBpZCwgdGFzaykge1xuICAgIHRoaXMuYWRkVG9TY2hlZHVsZXIocGlkLCAoKSA9PiB7XG4gICAgICB0YXNrKCk7XG4gICAgfSk7XG4gIH1cblxuICBzY2hlZHVsZUZ1dHVyZShwaWQsIGR1ZVRpbWUsIHRhc2spIHtcbiAgICB0aGlzLmFkZFRvU2NoZWR1bGVyKHBpZCwgKCkgPT4ge1xuICAgICAgdGFzaygpO1xuICAgIH0sIGR1ZVRpbWUpO1xuICB9XG59XG5cbmxldCBwcm9jZXNzX2NvdW50ZXIgPSAtMTtcblxuY2xhc3MgUElEIHtcbiAgY29uc3RydWN0b3IoKSB7XG4gICAgcHJvY2Vzc19jb3VudGVyID0gcHJvY2Vzc19jb3VudGVyICsgMTtcbiAgICB0aGlzLmlkID0gcHJvY2Vzc19jb3VudGVyO1xuICB9XG5cbiAgdG9TdHJpbmcoKSB7XG4gICAgcmV0dXJuIFwiUElEIzwwLlwiICsgdGhpcy5pZCArIFwiLjA+XCI7XG4gIH1cbn1cblxuY2xhc3MgUHJvY2Vzc1N5c3RlbSB7XG5cbiAgY29uc3RydWN0b3IoKSB7XG4gICAgdGhpcy5waWRzID0gbmV3IE1hcCgpO1xuICAgIHRoaXMubWFpbGJveGVzID0gbmV3IE1hcCgpO1xuICAgIHRoaXMubmFtZXMgPSBuZXcgTWFwKCk7XG4gICAgdGhpcy5saW5rcyA9IG5ldyBNYXAoKTtcblxuICAgIGNvbnN0IHRocm90dGxlID0gNTsgLy9tcyBiZXR3ZWVuIHNjaGVkdWxlZCB0YXNrc1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzID0gbnVsbDtcbiAgICB0aGlzLnNjaGVkdWxlciA9IG5ldyBTY2hlZHVsZXIodGhyb3R0bGUpO1xuICAgIHRoaXMuc3VzcGVuZGVkID0gbmV3IE1hcCgpO1xuXG4gICAgbGV0IHByb2Nlc3Nfc3lzdGVtX3Njb3BlID0gdGhpcztcbiAgICB0aGlzLm1haW5fcHJvY2Vzc19waWQgPSB0aGlzLnNwYXduKGZ1bmN0aW9uKiAoKSB7XG4gICAgICB3aGlsZSAodHJ1ZSkge1xuICAgICAgICB5aWVsZCBwcm9jZXNzX3N5c3RlbV9zY29wZS5zbGVlcCgxMDAwMCk7XG4gICAgICB9XG4gICAgfSk7XG4gICAgdGhpcy5zZXRfY3VycmVudCh0aGlzLm1haW5fcHJvY2Vzc19waWQpO1xuICB9XG5cbiAgc3RhdGljICpydW4oZnVuLCBhcmdzLCBjb250ZXh0ID0gbnVsbCkge1xuICAgIGlmIChmdW4uY29uc3RydWN0b3IubmFtZSA9PT0gXCJHZW5lcmF0b3JGdW5jdGlvblwiKSB7XG4gICAgICByZXR1cm4geWllbGQqIGZ1bi5hcHBseShjb250ZXh0LCBhcmdzKTtcbiAgICB9IGVsc2Uge1xuICAgICAgcmV0dXJuIGZ1bi5hcHBseShjb250ZXh0LCBhcmdzKTtcbiAgICB9XG4gIH1cblxuICBzcGF3biguLi5hcmdzKSB7XG4gICAgaWYgKGFyZ3MubGVuZ3RoID09PSAxKSB7XG4gICAgICBsZXQgZnVuID0gYXJnc1swXTtcbiAgICAgIHJldHVybiB0aGlzLmFkZF9wcm9jKGZ1biwgW10sIGZhbHNlKS5waWQ7XG4gICAgfSBlbHNlIGlmIChhcmdzLmxlbmd0aCA9PT0gMykge1xuICAgICAgbGV0IG1vZCA9IGFyZ3NbMF07XG4gICAgICBsZXQgZnVuID0gYXJnc1sxXTtcbiAgICAgIGxldCB0aGVfYXJncyA9IGFyZ3NbMl07XG5cbiAgICAgIHJldHVybiB0aGlzLmFkZF9wcm9jKG1vZFtmdW5dLCB0aGVfYXJncywgZmFsc2UpLnBpZDtcbiAgICB9XG4gIH1cblxuICBzcGF3bl9saW5rKC4uLmFyZ3MpIHtcbiAgICBpZiAoYXJncy5sZW5ndGggPT09IDEpIHtcbiAgICAgIGxldCBmdW4gPSBhcmdzWzBdO1xuICAgICAgcmV0dXJuIHRoaXMuYWRkX3Byb2MoZnVuLCBbXSwgdHJ1ZSkucGlkO1xuICAgIH0gZWxzZSBpZiAoYXJncy5sZW5ndGggPT09IDMpIHtcbiAgICAgIGxldCBtb2QgPSBhcmdzWzBdO1xuICAgICAgbGV0IGZ1biA9IGFyZ3NbMV07XG4gICAgICBsZXQgdGhlX2FyZ3MgPSBhcmdzWzJdO1xuXG4gICAgICByZXR1cm4gdGhpcy5hZGRfcHJvYyhtb2RbZnVuXSwgdGhlX2FyZ3MsIHRydWUpLnBpZDtcbiAgICB9XG4gIH1cblxuICBsaW5rKHBpZCkge1xuICAgIHRoaXMubGlua3MuZ2V0KHRoaXMucGlkKCkpLmFkZChwaWQpO1xuICAgIHRoaXMubGlua3MuZ2V0KHBpZCkuYWRkKHRoaXMucGlkKCkpO1xuICB9XG5cbiAgdW5saW5rKHBpZCkge1xuICAgIHRoaXMubGlua3MuZ2V0KHRoaXMucGlkKCkpLmRlbGV0ZShwaWQpO1xuICAgIHRoaXMubGlua3MuZ2V0KHBpZCkuZGVsZXRlKHRoaXMucGlkKCkpO1xuICB9XG5cbiAgc2V0X2N1cnJlbnQoaWQpIHtcbiAgICBsZXQgcGlkID0gdGhpcy5waWRvZihpZCk7XG4gICAgaWYgKHBpZCAhPT0gbnVsbCkge1xuICAgICAgdGhpcy5jdXJyZW50X3Byb2Nlc3MgPSB0aGlzLnBpZHMuZ2V0KHBpZCk7XG4gICAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5zdGF0dXMgPSBTdGF0ZXMuUlVOTklORztcbiAgICB9XG4gIH1cblxuICBhZGRfcHJvYyhmdW4sIGFyZ3MsIGxpbmtlZCkge1xuICAgIGxldCBuZXdwaWQgPSBuZXcgUElEKCk7XG4gICAgbGV0IG1haWxib3ggPSBuZXcgTWFpbGJveCgpO1xuICAgIGxldCBuZXdwcm9jID0gbmV3IFByb2Nlc3MobmV3cGlkLCBmdW4sIGFyZ3MsIG1haWxib3gsIHRoaXMpO1xuXG4gICAgdGhpcy5waWRzLnNldChuZXdwaWQsIG5ld3Byb2MpO1xuICAgIHRoaXMubWFpbGJveGVzLnNldChuZXdwaWQsIG1haWxib3gpO1xuICAgIHRoaXMubGlua3Muc2V0KG5ld3BpZCwgbmV3IFNldCgpKTtcblxuICAgIGlmIChsaW5rZWQpIHtcbiAgICAgIHRoaXMubGluayhuZXdwaWQpO1xuICAgIH1cblxuICAgIG5ld3Byb2Muc3RhcnQoKTtcbiAgICByZXR1cm4gbmV3cHJvYztcbiAgfVxuXG4gIHJlbW92ZV9wcm9jKHBpZCwgZXhpdHJlYXNvbikge1xuICAgIHRoaXMucGlkcy5kZWxldGUocGlkKTtcbiAgICB0aGlzLnVucmVnaXN0ZXIocGlkKTtcbiAgICB0aGlzLnNjaGVkdWxlci5yZW1vdmVQaWQocGlkKTtcblxuICAgIGlmICh0aGlzLmxpbmtzLmhhcyhwaWQpKSB7XG4gICAgICBmb3IgKGxldCBsaW5rcGlkIG9mIHRoaXMubGlua3MuZ2V0KHBpZCkpIHtcbiAgICAgICAgdGhpcy5leGl0KGxpbmtwaWQsIGV4aXRyZWFzb24pO1xuICAgICAgICB0aGlzLmxpbmtzLmdldChsaW5rcGlkKS5kZWxldGUocGlkKTtcbiAgICAgIH1cblxuICAgICAgdGhpcy5saW5rcy5kZWxldGUocGlkKTtcbiAgICB9XG4gIH1cblxuICByZWdpc3RlcihuYW1lLCBwaWQpIHtcbiAgICBpZiAoIXRoaXMubmFtZXMuaGFzKG5hbWUpKSB7XG4gICAgICB0aGlzLm5hbWVzLnNldChuYW1lLCBwaWQpO1xuICAgIH0gZWxzZSB7XG4gICAgICB0aHJvdyBuZXcgRXJyb3IoXCJOYW1lIGlzIGFscmVhZHkgcmVnaXN0ZXJlZCB0byBhbm90aGVyIHByb2Nlc3NcIik7XG4gICAgfVxuICB9XG5cbiAgcmVnaXN0ZXJlZChuYW1lKSB7XG4gICAgcmV0dXJuIHRoaXMubmFtZXMuaGFzKG5hbWUpID8gdGhpcy5uYW1lcy5nZXQobmFtZSkgOiBudWxsO1xuICB9XG5cbiAgdW5yZWdpc3RlcihwaWQpIHtcbiAgICBmb3IgKGxldCBuYW1lIG9mIHRoaXMubmFtZXMua2V5cygpKSB7XG4gICAgICBpZiAodGhpcy5uYW1lcy5oYXMobmFtZSkgJiYgdGhpcy5uYW1lcy5nZXQobmFtZSkgPT09IHBpZCkge1xuICAgICAgICB0aGlzLm5hbWVzLmRlbGV0ZShuYW1lKTtcbiAgICAgIH1cbiAgICB9XG4gIH1cblxuICBwaWQoKSB7XG4gICAgcmV0dXJuIHRoaXMuY3VycmVudF9wcm9jZXNzLnBpZDtcbiAgfVxuXG4gIHBpZG9mKGlkKSB7XG4gICAgaWYgKGlkIGluc3RhbmNlb2YgUElEKSB7XG4gICAgICByZXR1cm4gdGhpcy5waWRzLmhhcyhpZCkgPyBpZCA6IG51bGw7XG4gICAgfSBlbHNlIGlmIChpZCBpbnN0YW5jZW9mIFByb2Nlc3MpIHtcbiAgICAgIHJldHVybiBpZC5waWQ7XG4gICAgfSBlbHNlIHtcbiAgICAgIGxldCBwaWQgPSB0aGlzLnJlZ2lzdGVyZWQoaWQpO1xuICAgICAgaWYgKHBpZCA9PT0gbnVsbCkgdGhyb3cgXCJQcm9jZXNzIG5hbWUgbm90IHJlZ2lzdGVyZWQ6IFwiICsgaWQgKyBcIiAoXCIgKyB0eXBlb2YgaWQgKyBcIilcIjtcbiAgICAgIHJldHVybiBwaWQ7XG4gICAgfVxuICB9XG5cbiAgc2VuZChpZCwgbXNnKSB7XG4gICAgY29uc3QgcGlkID0gdGhpcy5waWRvZihpZCk7XG5cbiAgICBpZiAocGlkKSB7XG4gICAgICB0aGlzLm1haWxib3hlcy5nZXQocGlkKS5kZWxpdmVyKG1zZyk7XG5cbiAgICAgIGlmICh0aGlzLnN1c3BlbmRlZC5oYXMocGlkKSkge1xuICAgICAgICBsZXQgZnVuID0gdGhpcy5zdXNwZW5kZWQuZ2V0KHBpZCk7XG4gICAgICAgIHRoaXMuc3VzcGVuZGVkLmRlbGV0ZShwaWQpO1xuICAgICAgICB0aGlzLnNjaGVkdWxlKGZ1bik7XG4gICAgICB9XG4gICAgfVxuXG4gICAgcmV0dXJuIG1zZztcbiAgfVxuXG4gIHJlY2VpdmUoZnVuLCB0aW1lb3V0ID0gMCwgdGltZW91dEZuID0gKCkgPT4gdHJ1ZSkge1xuICAgIGxldCBEYXRlVGltZW91dCA9IG51bGw7XG5cbiAgICBpZiAodGltZW91dCA9PT0gMCB8fCB0aW1lb3V0ID09PSBJbmZpbml0eSkge1xuICAgICAgRGF0ZVRpbWVvdXQgPSBudWxsO1xuICAgIH0gZWxzZSB7XG4gICAgICBEYXRlVGltZW91dCA9IERhdGUubm93KCkgKyB0aW1lb3V0O1xuICAgIH1cblxuICAgIHJldHVybiBbU3RhdGVzLlJFQ0VJVkUsIGZ1biwgRGF0ZVRpbWVvdXQsIHRpbWVvdXRGbl07XG4gIH1cblxuICBzbGVlcChkdXJhdGlvbikge1xuICAgIHJldHVybiBbU3RhdGVzLlNMRUVQLCBkdXJhdGlvbl07XG4gIH1cblxuICBzdXNwZW5kKGZ1bikge1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLnN0YXR1cyA9IFN0YXRlcy5TVVNQRU5ERUQ7XG4gICAgdGhpcy5zdXNwZW5kZWQuc2V0KHRoaXMuY3VycmVudF9wcm9jZXNzLnBpZCwgZnVuKTtcbiAgfVxuXG4gIGRlbGF5KGZ1biwgdGltZSkge1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLnN0YXR1cyA9IFN0YXRlcy5TTEVFUElORztcbiAgICB0aGlzLnNjaGVkdWxlci5zY2hlZHVsZUZ1dHVyZSh0aGlzLmN1cnJlbnRfcHJvY2Vzcy5waWQsIHRpbWUsIGZ1bik7XG4gIH1cblxuICBzY2hlZHVsZShmdW4sIHBpZCkge1xuICAgIGNvbnN0IHRoZV9waWQgPSBwaWQgIT0gbnVsbCA/IHBpZCA6IHRoaXMuY3VycmVudF9wcm9jZXNzLnBpZDtcbiAgICB0aGlzLnNjaGVkdWxlci5zY2hlZHVsZSh0aGVfcGlkLCBmdW4pO1xuICB9XG5cbiAgZXhpdChvbmUsIHR3bykge1xuICAgIGlmICh0d28pIHtcbiAgICAgIGxldCBwaWQgPSBvbmU7XG4gICAgICBsZXQgcmVhc29uID0gdHdvO1xuXG4gICAgICBsZXQgcHJvY2VzcyA9IHRoaXMucGlkcy5nZXQodGhpcy5waWRvZihwaWQpKTtcbiAgICAgIGlmIChwcm9jZXNzICYmIHByb2Nlc3MuaXNfdHJhcHBpbmdfZXhpdHMoKSB8fCByZWFzb24gPT09IFN0YXRlcy5LSUxMIHx8IHJlYXNvbiA9PT0gU3RhdGVzLk5PUk1BTCkge1xuICAgICAgICB0aGlzLm1haWxib3hlcy5nZXQocHJvY2Vzcy5waWQpLmRlbGl2ZXIoW1N0YXRlcy5FWElULCB0aGlzLnBpZCgpLCByZWFzb25dKTtcbiAgICAgIH0gZWxzZSB7XG4gICAgICAgIHByb2Nlc3Muc2lnbmFsKHJlYXNvbik7XG4gICAgICB9XG4gICAgfSBlbHNlIHtcbiAgICAgIGxldCByZWFzb24gPSBvbmU7XG4gICAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5zaWduYWwocmVhc29uKTtcbiAgICB9XG4gIH1cblxuICBlcnJvcihyZWFzb24pIHtcbiAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5zaWduYWwocmVhc29uKTtcbiAgfVxuXG4gIHByb2Nlc3NfZmxhZyhmbGFnLCB2YWx1ZSkge1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLnByb2Nlc3NfZmxhZyhmbGFnLCB2YWx1ZSk7XG4gIH1cblxuICBwdXQoa2V5LCB2YWx1ZSkge1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3Rba2V5XSA9IHZhbHVlO1xuICB9XG5cbiAgZ2V0KGtleSkge1xuICAgIGlmIChrZXkgIT0gbnVsbCkge1xuICAgICAgcmV0dXJuIHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3Rba2V5XTtcbiAgICB9IGVsc2Uge1xuICAgICAgcmV0dXJuIHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3Q7XG4gICAgfVxuICB9XG5cbiAgZ2V0X2tleXMoKSB7XG4gICAgcmV0dXJuIE9iamVjdC5rZXlzKHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3QpO1xuICB9XG5cbiAgZXJhc2Uoa2V5KSB7XG4gICAgaWYgKGtleSAhPSBudWxsKSB7XG4gICAgICBkZWxldGUgdGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdFtrZXldO1xuICAgIH0gZWxzZSB7XG4gICAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0ID0ge307XG4gICAgfVxuICB9XG59XG5cbnZhciBpbmRleCA9IHtcbiAgUHJvY2Vzc1N5c3RlbSxcbiAgR2VuU2VydmVyXG59O1xuXG5leHBvcnQgZGVmYXVsdCBpbmRleDsiXSwic291cmNlUm9vdCI6Ii9zb3VyY2UvIn0=
