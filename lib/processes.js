"use strict";

var _arguments = arguments;

var _slicedToArray = (function () { function sliceIterator(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"]) _i["return"](); } finally { if (_d) throw _e; } } return _arr; } return function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { return sliceIterator(arr, i); } else { throw new TypeError("Invalid attempt to destructure non-iterable instance"); } }; })();

var _createClass = (function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; })();

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _marked = [call, cast].map(regeneratorRuntime.mark);

function _typeof(obj) { return obj && typeof Symbol !== "undefined" && obj.constructor === Symbol ? "symbol" : typeof obj; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var Mailbox = (function () {
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

var Process = (function () {
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
      var function_scope = this;
      var machine = this.main();

      this.system.schedule(function () {
        function_scope.system.set_current(function_scope.pid);
        function_scope.run(machine, machine.next());
      }, this.pid);
    }
  }, {
    key: "main",
    value: regeneratorRuntime.mark(function main() {
      var retval;
      return regeneratorRuntime.wrap(function main$(_context) {
        while (1) {
          switch (_context.prev = _context.next) {
            case 0:
              retval = States.NORMAL;
              _context.prev = 1;
              return _context.delegateYield(this.func.apply(null, this.args), "t0", 3);

            case 3:
              _context.next = 9;
              break;

            case 5:
              _context.prev = 5;
              _context.t1 = _context["catch"](1);

              console.error(_context.t1);
              retval = _context.t1;

            case 9:

              this.system.exit(retval);

            case 10:
            case "end":
              return _context.stop();
          }
        }
      }, main, this, [[1, 5]]);
    })
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
      var value = States.NOMATCH;
      var messages = this.mailbox.get();

      for (var i = 0; i < messages.length; i++) {
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
      var _this = this;

      var function_scope = this;

      if (!step.done) {
        (function () {
          var value = step.value;

          if (is_sleep(value)) {

            _this.system.delay(function () {
              function_scope.system.set_current(function_scope.pid);
              function_scope.run(machine, machine.next());
            }, value[1]);
          } else if (is_receive(value) && receive_timed_out(value)) {
            (function () {

              var result = value[3]();

              _this.system.schedule(function () {
                function_scope.system.set_current(function_scope.pid);
                function_scope.run(machine, machine.next(result));
              });
            })();
          } else if (is_receive(value)) {
            (function () {

              var result = function_scope.receive(value[1]);

              if (result === States.NOMATCH) {
                _this.system.suspend(function () {
                  function_scope.system.set_current(function_scope.pid);
                  function_scope.run(machine, step);
                });
              } else {
                _this.system.schedule(function () {
                  function_scope.system.set_current(function_scope.pid);
                  function_scope.run(machine, machine.next(result));
                });
              }
            })();
          } else {
            _this.system.schedule(function () {
              function_scope.system.set_current(function_scope.pid);
              function_scope.run(machine, machine.next(value));
            });
          }
        })();
      }
    }
  }]);

  return Process;
})();

var MAX_REDUCTIONS_PER_PROCESS = 8;

var ProcessQueue = (function () {
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

var Scheduler = (function () {
  function Scheduler() {
    var throttle = arguments.length <= 0 || arguments[0] === undefined ? 0 : arguments[0];

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
      var _this2 = this;

      if (this.isRunning) {
        this.invokeLater(function () {
          _this2.run();
        });
      } else {
        var _iteratorNormalCompletion = true;
        var _didIteratorError = false;
        var _iteratorError = undefined;

        try {
          for (var _iterator = this.queues[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
            var _step$value = _slicedToArray(_step.value, 2);

            var pid = _step$value[0];
            var queue = _step$value[1];

            var reductions = 0;
            while (queue && !queue.empty() && reductions < MAX_REDUCTIONS_PER_PROCESS) {
              var task = queue.next();
              this.isRunning = true;

              var result = undefined;

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
        } catch (err) {
          _didIteratorError = true;
          _iteratorError = err;
        } finally {
          try {
            if (!_iteratorNormalCompletion && _iterator.return) {
              _iterator.return();
            }
          } finally {
            if (_didIteratorError) {
              throw _iteratorError;
            }
          }
        }

        this.invokeLater(function () {
          _this2.run();
        });
      }
    }
  }, {
    key: "addToScheduler",
    value: function addToScheduler(pid, task) {
      var _this3 = this;

      var dueTime = arguments.length <= 2 || arguments[2] === undefined ? 0 : arguments[2];

      if (dueTime === 0) {
        this.invokeLater(function () {
          _this3.addToQueue(pid, task);
        });
      } else {
        setTimeout(function () {
          _this3.addToQueue(pid, task);
        }, dueTime);
      }
    }
  }, {
    key: "schedule",
    value: function schedule(pid, task) {
      this.addToScheduler(pid, function () {
        task();
      });
    }
  }, {
    key: "scheduleFuture",
    value: function scheduleFuture(pid, dueTime, task) {
      this.addToScheduler(pid, function () {
        task();
      }, dueTime);
    }
  }]);

  return Scheduler;
})();

var process_counter = -1;

var PID = (function () {
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

var ProcessSystem = (function () {
  function ProcessSystem() {
    _classCallCheck(this, ProcessSystem);

    this.pids = new Map();
    this.mailboxes = new Map();
    this.names = new Map();
    this.links = new Map();

    var throttle = 5; //ms between scheduled tasks
    this.current_process = null;
    this.scheduler = new Scheduler(throttle);
    this.suspended = new Map();

    var process_system_scope = this;
    this.main_process_pid = this.spawn(regeneratorRuntime.mark(function _callee() {
      return regeneratorRuntime.wrap(function _callee$(_context2) {
        while (1) {
          switch (_context2.prev = _context2.next) {
            case 0:
              if (!true) {
                _context2.next = 5;
                break;
              }

              _context2.next = 3;
              return process_system_scope.sleep(10000);

            case 3:
              _context2.next = 0;
              break;

            case 5:
            case "end":
              return _context2.stop();
          }
        }
      }, _callee, this);
    }));
    this.set_current(this.main_process_pid);
  }

  _createClass(ProcessSystem, [{
    key: "spawn",
    value: function spawn() {
      for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
        args[_key] = arguments[_key];
      }

      if (args.length === 1) {
        var fun = args[0];
        return this.add_proc(fun, [], false).pid;
      } else if (args.length === 3) {
        var mod = args[0];
        var fun = args[1];
        var the_args = args[2];

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
        var fun = args[0];
        return this.add_proc(fun, [], true).pid;
      } else if (args.length === 3) {
        var mod = args[0];
        var fun = args[1];
        var the_args = args[2];

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
      var pid = this.pidof(id);
      if (pid !== null) {
        this.current_process = this.pids.get(pid);
        this.current_process.status = States.RUNNING;
      }
    }
  }, {
    key: "add_proc",
    value: function add_proc(fun, args, linked) {
      var newpid = new PID();
      var mailbox = new Mailbox();
      var newproc = new Process(newpid, fun, args, mailbox, this);

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
        var _iteratorNormalCompletion2 = true;
        var _didIteratorError2 = false;
        var _iteratorError2 = undefined;

        try {
          for (var _iterator2 = this.links.get(pid)[Symbol.iterator](), _step2; !(_iteratorNormalCompletion2 = (_step2 = _iterator2.next()).done); _iteratorNormalCompletion2 = true) {
            var linkpid = _step2.value;

            this.exit(linkpid, exitreason);
            this.links.get(linkpid).delete(pid);
          }
        } catch (err) {
          _didIteratorError2 = true;
          _iteratorError2 = err;
        } finally {
          try {
            if (!_iteratorNormalCompletion2 && _iterator2.return) {
              _iterator2.return();
            }
          } finally {
            if (_didIteratorError2) {
              throw _iteratorError2;
            }
          }
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
      var _iteratorNormalCompletion3 = true;
      var _didIteratorError3 = false;
      var _iteratorError3 = undefined;

      try {
        for (var _iterator3 = this.names.keys()[Symbol.iterator](), _step3; !(_iteratorNormalCompletion3 = (_step3 = _iterator3.next()).done); _iteratorNormalCompletion3 = true) {
          var name = _step3.value;

          if (this.names.has(name) && this.names.get(name) === pid) {
            this.names.delete(name);
          }
        }
      } catch (err) {
        _didIteratorError3 = true;
        _iteratorError3 = err;
      } finally {
        try {
          if (!_iteratorNormalCompletion3 && _iterator3.return) {
            _iterator3.return();
          }
        } finally {
          if (_didIteratorError3) {
            throw _iteratorError3;
          }
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
        var pid = this.registered(id);
        if (pid === null) throw "Process name not registered: " + id + " (" + (typeof id === "undefined" ? "undefined" : _typeof(id)) + ")";
        return pid;
      }
    }
  }, {
    key: "send",
    value: function send(id, msg) {
      var pid = this.pidof(id);

      if (pid) {
        this.mailboxes.get(pid).deliver(msg);

        if (this.suspended.has(pid)) {
          var fun = this.suspended.get(pid);
          this.suspended.delete(pid);
          this.schedule(fun);
        }
      }

      return msg;
    }
  }, {
    key: "receive",
    value: function receive(fun) {
      var timeout = arguments.length <= 1 || arguments[1] === undefined ? 0 : arguments[1];
      var timeoutFn = arguments.length <= 2 || arguments[2] === undefined ? function () {
        return true;
      } : arguments[2];

      var DateTimeout = null;

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
      var the_pid = pid != null ? pid : this.current_process.pid;
      this.scheduler.schedule(the_pid, fun);
    }
  }, {
    key: "exit",
    value: function exit(one, two) {
      if (two) {
        var pid = one;
        var reason = two;

        var process = this.pids.get(this.pidof(pid));
        if (process && process.is_trapping_exits() || reason === States.KILL || reason === States.NORMAL) {
          this.mailboxes.get(process.pid).deliver([States.EXIT, this.pid(), reason]);
        } else {
          process.signal(reason);
        }
      } else {
        var reason = one;
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
    value: regeneratorRuntime.mark(function run(fun, args) {
      var context = _arguments.length <= 2 || _arguments[2] === undefined ? null : _arguments[2];
      return regeneratorRuntime.wrap(function run$(_context3) {
        while (1) {
          switch (_context3.prev = _context3.next) {
            case 0:
              if (!(fun.constructor.name === "GeneratorFunction")) {
                _context3.next = 5;
                break;
              }

              return _context3.delegateYield(fun.apply(context, args), "t0", 2);

            case 2:
              return _context3.abrupt("return", _context3.t0);

            case 5:
              return _context3.abrupt("return", fun.apply(context, args));

            case 6:
            case "end":
              return _context3.stop();
          }
        }
      }, run, this);
    })
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
  return regeneratorRuntime.mark(function _callee2() {
    var _module$init$apply, _module$init$apply2, ok, state;

    return regeneratorRuntime.wrap(function _callee2$(_context4) {
      while (1) {
        switch (_context4.prev = _context4.next) {
          case 0:
            _module$init$apply = module.init.apply(null, [args]);
            _module$init$apply2 = _slicedToArray(_module$init$apply, 2);
            ok = _module$init$apply2[0];
            state = _module$init$apply2[1];
            _context4.next = 6;
            return self.system.put("state", state);

          case 6:
            _context4.prev = 6;

          case 7:
            if (!true) {
              _context4.next = 12;
              break;
            }

            _context4.next = 10;
            return self.system.receive(function (args) {
              var command = args[0];

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

          case 10:
            _context4.next = 7;
            break;

          case 12:
            _context4.next = 18;
            break;

          case 14:
            _context4.prev = 14;
            _context4.t0 = _context4["catch"](6);

            if (!(_context4.t0 !== "stop")) {
              _context4.next = 18;
              break;
            }

            throw _context4.t0;

          case 18:
          case "end":
            return _context4.stop();
        }
      }
    }, _callee2, this, [[6, 14]]);
  });
}

function call(server, request) {
  return regeneratorRuntime.wrap(function call$(_context5) {
    while (1) switch (_context5.prev = _context5.next) {
      case 0:
        self.system.send(server, ["call", request, self.system.pid()]);

        _context5.next = 3;
        return self.system.receive(function (args) {
          return args;
        });

      case 3:
        return _context5.abrupt("return", _context5.sent);

      case 4:
      case "end":
        return _context5.stop();
    }
  }, _marked[0], this);
}

function cast(server, request) {
  return regeneratorRuntime.wrap(function cast$(_context6) {
    while (1) switch (_context6.prev = _context6.next) {
      case 0:
        self.system.send(server, ["cast", request, self.system.pid()]);

        _context6.next = 3;
        return self.system.receive(function (args) {
          return args;
        });

      case 3:
        return _context6.abrupt("return", _context6.sent);

      case 4:
      case "end":
        return _context6.stop();
    }
  }, _marked[1], this);
}

function stop(server) {
  self.system.send(server, ["stop"]);
}

var gen_server = { start: start, start_link: start_link, call: call, cast: cast, stop: stop };

exports.ProcessSystem = ProcessSystem;
exports.GenServer = gen_server;
//# sourceMappingURL=data:application/json;base64,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ZXguanMiLCJzb3VyY2VzQ29udGVudCI6WyJjbGFzcyBNYWlsYm94IHtcblxuICBjb25zdHJ1Y3RvcigpIHtcbiAgICB0aGlzLm1lc3NhZ2VzID0gW107XG4gIH1cblxuICBkZWxpdmVyKG1lc3NhZ2UpIHtcbiAgICB0aGlzLm1lc3NhZ2VzLnB1c2gobWVzc2FnZSk7XG4gICAgcmV0dXJuIG1lc3NhZ2U7XG4gIH1cblxuICBnZXQoKSB7XG4gICAgcmV0dXJuIHRoaXMubWVzc2FnZXM7XG4gIH1cblxuICBpc0VtcHR5KCkge1xuICAgIHJldHVybiB0aGlzLm1lc3NhZ2VzLmxlbmd0aCA9PT0gMDtcbiAgfVxuXG4gIHJlbW92ZUF0KGluZGV4KSB7XG4gICAgdGhpcy5tZXNzYWdlcy5zcGxpY2UoaW5kZXgsIDEpO1xuICB9XG59XG5cbnZhciBTdGF0ZXMgPSB7XG4gIE5PUk1BTDogU3ltYm9sLmZvcihcIm5vcm1hbFwiKSxcbiAgS0lMTDogU3ltYm9sLmZvcihcImtpbGxcIiksXG4gIFNVU1BFTkQ6IFN5bWJvbC5mb3IoXCJzdXNwZW5kXCIpLFxuICBDT05USU5VRTogU3ltYm9sLmZvcihcImNvbnRpbnVlXCIpLFxuICBSRUNFSVZFOiBTeW1ib2wuZm9yKFwicmVjZWl2ZVwiKSxcbiAgU0VORDogU3ltYm9sLmZvcihcInNlbmRcIiksXG4gIFNMRUVQSU5HOiBTeW1ib2wuZm9yKFwic2xlZXBpbmdcIiksXG4gIFJVTk5JTkc6IFN5bWJvbC5mb3IoXCJydW5uaW5nXCIpLFxuICBTVVNQRU5ERUQ6IFN5bWJvbC5mb3IoXCJzdXNwZW5kZWRcIiksXG4gIFNUT1BQRUQ6IFN5bWJvbC5mb3IoXCJzdG9wcGVkXCIpLFxuICBTTEVFUDogU3ltYm9sLmZvcihcInNsZWVwXCIpLFxuICBFWElUOiBTeW1ib2wuZm9yKFwiZXhpdFwiKSxcbiAgTk9NQVRDSDogU3ltYm9sLmZvcihcIm5vX21hdGNoXCIpXG59O1xuXG5mdW5jdGlvbiBpc19zbGVlcCh2YWx1ZSkge1xuICByZXR1cm4gQXJyYXkuaXNBcnJheSh2YWx1ZSkgJiYgdmFsdWVbMF0gPT09IFN0YXRlcy5TTEVFUDtcbn1cblxuZnVuY3Rpb24gaXNfcmVjZWl2ZSh2YWx1ZSkge1xuICByZXR1cm4gQXJyYXkuaXNBcnJheSh2YWx1ZSkgJiYgdmFsdWVbMF0gPT09IFN0YXRlcy5SRUNFSVZFO1xufVxuXG5mdW5jdGlvbiByZWNlaXZlX3RpbWVkX291dCh2YWx1ZSkge1xuICByZXR1cm4gdmFsdWVbMl0gIT0gbnVsbCAmJiB2YWx1ZVsyXSA8IERhdGUubm93KCk7XG59XG5cbmNsYXNzIFByb2Nlc3Mge1xuXG4gIGNvbnN0cnVjdG9yKHBpZCwgZnVuYywgYXJncywgbWFpbGJveCwgc3lzdGVtKSB7XG4gICAgdGhpcy5waWQgPSBwaWQ7XG4gICAgdGhpcy5mdW5jID0gZnVuYztcbiAgICB0aGlzLmFyZ3MgPSBhcmdzO1xuICAgIHRoaXMubWFpbGJveCA9IG1haWxib3g7XG4gICAgdGhpcy5zeXN0ZW0gPSBzeXN0ZW07XG4gICAgdGhpcy5zdGF0dXMgPSBTdGF0ZXMuU1RPUFBFRDtcbiAgICB0aGlzLmRpY3QgPSB7fTtcbiAgICB0aGlzLmZsYWdzID0ge307XG4gIH1cblxuICBzdGFydCgpIHtcbiAgICBjb25zdCBmdW5jdGlvbl9zY29wZSA9IHRoaXM7XG4gICAgbGV0IG1hY2hpbmUgPSB0aGlzLm1haW4oKTtcblxuICAgIHRoaXMuc3lzdGVtLnNjaGVkdWxlKGZ1bmN0aW9uICgpIHtcbiAgICAgIGZ1bmN0aW9uX3Njb3BlLnN5c3RlbS5zZXRfY3VycmVudChmdW5jdGlvbl9zY29wZS5waWQpO1xuICAgICAgZnVuY3Rpb25fc2NvcGUucnVuKG1hY2hpbmUsIG1hY2hpbmUubmV4dCgpKTtcbiAgICB9LCB0aGlzLnBpZCk7XG4gIH1cblxuICAqbWFpbigpIHtcbiAgICBsZXQgcmV0dmFsID0gU3RhdGVzLk5PUk1BTDtcblxuICAgIHRyeSB7XG4gICAgICB5aWVsZCogdGhpcy5mdW5jLmFwcGx5KG51bGwsIHRoaXMuYXJncyk7XG4gICAgfSBjYXRjaCAoZSkge1xuICAgICAgY29uc29sZS5lcnJvcihlKTtcbiAgICAgIHJldHZhbCA9IGU7XG4gICAgfVxuXG4gICAgdGhpcy5zeXN0ZW0uZXhpdChyZXR2YWwpO1xuICB9XG5cbiAgcHJvY2Vzc19mbGFnKGZsYWcsIHZhbHVlKSB7XG4gICAgdGhpcy5mbGFnc1tmbGFnXSA9IHZhbHVlO1xuICB9XG5cbiAgaXNfdHJhcHBpbmdfZXhpdHMoKSB7XG4gICAgcmV0dXJuIHRoaXMuZmxhZ3NbU3ltYm9sLmZvcihcInRyYXBfZXhpdFwiKV0gJiYgdGhpcy5mbGFnc1tTeW1ib2wuZm9yKFwidHJhcF9leGl0XCIpXSA9PSB0cnVlO1xuICB9XG5cbiAgc2lnbmFsKHJlYXNvbikge1xuICAgIGlmIChyZWFzb24gIT09IFN0YXRlcy5OT1JNQUwpIHtcbiAgICAgIGNvbnNvbGUuZXJyb3IocmVhc29uKTtcbiAgICB9XG5cbiAgICB0aGlzLnN5c3RlbS5yZW1vdmVfcHJvYyh0aGlzLnBpZCwgcmVhc29uKTtcbiAgfVxuXG4gIHJlY2VpdmUoZnVuKSB7XG4gICAgbGV0IHZhbHVlID0gU3RhdGVzLk5PTUFUQ0g7XG4gICAgbGV0IG1lc3NhZ2VzID0gdGhpcy5tYWlsYm94LmdldCgpO1xuXG4gICAgZm9yIChsZXQgaSA9IDA7IGkgPCBtZXNzYWdlcy5sZW5ndGg7IGkrKykge1xuICAgICAgdHJ5IHtcbiAgICAgICAgdmFsdWUgPSBmdW4obWVzc2FnZXNbaV0pO1xuICAgICAgICBpZiAodmFsdWUgIT09IFN0YXRlcy5OT01BVENIKSB7XG4gICAgICAgICAgdGhpcy5tYWlsYm94LnJlbW92ZUF0KGkpO1xuICAgICAgICAgIGJyZWFrO1xuICAgICAgICB9XG4gICAgICB9IGNhdGNoIChlKSB7XG4gICAgICAgIHRoaXMuZXhpdChlKTtcbiAgICAgIH1cbiAgICB9XG5cbiAgICByZXR1cm4gdmFsdWU7XG4gIH1cblxuICBydW4obWFjaGluZSwgc3RlcCkge1xuICAgIGNvbnN0IGZ1bmN0aW9uX3Njb3BlID0gdGhpcztcblxuICAgIGlmICghc3RlcC5kb25lKSB7XG4gICAgICBsZXQgdmFsdWUgPSBzdGVwLnZhbHVlO1xuXG4gICAgICBpZiAoaXNfc2xlZXAodmFsdWUpKSB7XG5cbiAgICAgICAgdGhpcy5zeXN0ZW0uZGVsYXkoZnVuY3Rpb24gKCkge1xuICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnN5c3RlbS5zZXRfY3VycmVudChmdW5jdGlvbl9zY29wZS5waWQpO1xuICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnJ1bihtYWNoaW5lLCBtYWNoaW5lLm5leHQoKSk7XG4gICAgICAgIH0sIHZhbHVlWzFdKTtcbiAgICAgIH0gZWxzZSBpZiAoaXNfcmVjZWl2ZSh2YWx1ZSkgJiYgcmVjZWl2ZV90aW1lZF9vdXQodmFsdWUpKSB7XG5cbiAgICAgICAgbGV0IHJlc3VsdCA9IHZhbHVlWzNdKCk7XG5cbiAgICAgICAgdGhpcy5zeXN0ZW0uc2NoZWR1bGUoZnVuY3Rpb24gKCkge1xuICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnN5c3RlbS5zZXRfY3VycmVudChmdW5jdGlvbl9zY29wZS5waWQpO1xuICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnJ1bihtYWNoaW5lLCBtYWNoaW5lLm5leHQocmVzdWx0KSk7XG4gICAgICAgIH0pO1xuICAgICAgfSBlbHNlIGlmIChpc19yZWNlaXZlKHZhbHVlKSkge1xuXG4gICAgICAgIGxldCByZXN1bHQgPSBmdW5jdGlvbl9zY29wZS5yZWNlaXZlKHZhbHVlWzFdKTtcblxuICAgICAgICBpZiAocmVzdWx0ID09PSBTdGF0ZXMuTk9NQVRDSCkge1xuICAgICAgICAgIHRoaXMuc3lzdGVtLnN1c3BlbmQoZnVuY3Rpb24gKCkge1xuICAgICAgICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICAgICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgc3RlcCk7XG4gICAgICAgICAgfSk7XG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgdGhpcy5zeXN0ZW0uc2NoZWR1bGUoZnVuY3Rpb24gKCkge1xuICAgICAgICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICAgICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgbWFjaGluZS5uZXh0KHJlc3VsdCkpO1xuICAgICAgICAgIH0pO1xuICAgICAgICB9XG4gICAgICB9IGVsc2Uge1xuICAgICAgICB0aGlzLnN5c3RlbS5zY2hlZHVsZShmdW5jdGlvbiAoKSB7XG4gICAgICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICAgICAgZnVuY3Rpb25fc2NvcGUucnVuKG1hY2hpbmUsIG1hY2hpbmUubmV4dCh2YWx1ZSkpO1xuICAgICAgICB9KTtcbiAgICAgIH1cbiAgICB9XG4gIH1cbn1cblxuY29uc3QgTUFYX1JFRFVDVElPTlNfUEVSX1BST0NFU1MgPSA4O1xuXG5jbGFzcyBQcm9jZXNzUXVldWUge1xuICBjb25zdHJ1Y3RvcihwaWQpIHtcbiAgICB0aGlzLnBpZCA9IHBpZDtcbiAgICB0aGlzLnRhc2tzID0gW107XG4gIH1cblxuICBlbXB0eSgpIHtcbiAgICByZXR1cm4gdGhpcy50YXNrcy5sZW5ndGggPT09IDA7XG4gIH1cblxuICBhZGQodGFzaykge1xuICAgIHRoaXMudGFza3MucHVzaCh0YXNrKTtcbiAgfVxuXG4gIG5leHQoKSB7XG4gICAgcmV0dXJuIHRoaXMudGFza3Muc2hpZnQoKTtcbiAgfVxufVxuXG5jbGFzcyBTY2hlZHVsZXIge1xuICBjb25zdHJ1Y3Rvcih0aHJvdHRsZSA9IDApIHtcbiAgICB0aGlzLmlzUnVubmluZyA9IGZhbHNlO1xuICAgIHRoaXMuaW52b2tlTGF0ZXIgPSBmdW5jdGlvbiAoY2FsbGJhY2spIHtcbiAgICAgIHNldFRpbWVvdXQoY2FsbGJhY2ssIHRocm90dGxlKTtcbiAgICB9O1xuICAgIHRoaXMucXVldWVzID0gbmV3IE1hcCgpO1xuICAgIHRoaXMucnVuKCk7XG4gIH1cblxuICBhZGRUb1F1ZXVlKHBpZCwgdGFzaykge1xuICAgIGlmICghdGhpcy5xdWV1ZXMuaGFzKHBpZCkpIHtcbiAgICAgIHRoaXMucXVldWVzLnNldChwaWQsIG5ldyBQcm9jZXNzUXVldWUocGlkKSk7XG4gICAgfVxuXG4gICAgdGhpcy5xdWV1ZXMuZ2V0KHBpZCkuYWRkKHRhc2spO1xuICB9XG5cbiAgcmVtb3ZlUGlkKHBpZCkge1xuICAgIHRoaXMuaXNSdW5uaW5nID0gdHJ1ZTtcblxuICAgIHRoaXMucXVldWVzLmRlbGV0ZShwaWQpO1xuXG4gICAgdGhpcy5pc1J1bm5pbmcgPSBmYWxzZTtcbiAgfVxuXG4gIHJ1bigpIHtcbiAgICBpZiAodGhpcy5pc1J1bm5pbmcpIHtcbiAgICAgIHRoaXMuaW52b2tlTGF0ZXIoKCkgPT4ge1xuICAgICAgICB0aGlzLnJ1bigpO1xuICAgICAgfSk7XG4gICAgfSBlbHNlIHtcbiAgICAgIGZvciAobGV0IFtwaWQsIHF1ZXVlXSBvZiB0aGlzLnF1ZXVlcykge1xuICAgICAgICBsZXQgcmVkdWN0aW9ucyA9IDA7XG4gICAgICAgIHdoaWxlIChxdWV1ZSAmJiAhcXVldWUuZW1wdHkoKSAmJiByZWR1Y3Rpb25zIDwgTUFYX1JFRFVDVElPTlNfUEVSX1BST0NFU1MpIHtcbiAgICAgICAgICBsZXQgdGFzayA9IHF1ZXVlLm5leHQoKTtcbiAgICAgICAgICB0aGlzLmlzUnVubmluZyA9IHRydWU7XG5cbiAgICAgICAgICBsZXQgcmVzdWx0O1xuXG4gICAgICAgICAgdHJ5IHtcbiAgICAgICAgICAgIHJlc3VsdCA9IHRhc2soKTtcbiAgICAgICAgICB9IGNhdGNoIChlKSB7XG4gICAgICAgICAgICBjb25zb2xlLmVycm9yKGUpO1xuICAgICAgICAgICAgcmVzdWx0ID0gZTtcbiAgICAgICAgICB9XG5cbiAgICAgICAgICB0aGlzLmlzUnVubmluZyA9IGZhbHNlO1xuXG4gICAgICAgICAgaWYgKHJlc3VsdCBpbnN0YW5jZW9mIEVycm9yKSB7XG4gICAgICAgICAgICB0aHJvdyByZXN1bHQ7XG4gICAgICAgICAgfVxuXG4gICAgICAgICAgcmVkdWN0aW9ucysrO1xuICAgICAgICB9XG4gICAgICB9XG5cbiAgICAgIHRoaXMuaW52b2tlTGF0ZXIoKCkgPT4ge1xuICAgICAgICB0aGlzLnJ1bigpO1xuICAgICAgfSk7XG4gICAgfVxuICB9XG5cbiAgYWRkVG9TY2hlZHVsZXIocGlkLCB0YXNrLCBkdWVUaW1lID0gMCkge1xuICAgIGlmIChkdWVUaW1lID09PSAwKSB7XG4gICAgICB0aGlzLmludm9rZUxhdGVyKCgpID0+IHtcbiAgICAgICAgdGhpcy5hZGRUb1F1ZXVlKHBpZCwgdGFzayk7XG4gICAgICB9KTtcbiAgICB9IGVsc2Uge1xuICAgICAgc2V0VGltZW91dCgoKSA9PiB7XG4gICAgICAgIHRoaXMuYWRkVG9RdWV1ZShwaWQsIHRhc2spO1xuICAgICAgfSwgZHVlVGltZSk7XG4gICAgfVxuICB9XG5cbiAgc2NoZWR1bGUocGlkLCB0YXNrKSB7XG4gICAgdGhpcy5hZGRUb1NjaGVkdWxlcihwaWQsICgpID0+IHtcbiAgICAgIHRhc2soKTtcbiAgICB9KTtcbiAgfVxuXG4gIHNjaGVkdWxlRnV0dXJlKHBpZCwgZHVlVGltZSwgdGFzaykge1xuICAgIHRoaXMuYWRkVG9TY2hlZHVsZXIocGlkLCAoKSA9PiB7XG4gICAgICB0YXNrKCk7XG4gICAgfSwgZHVlVGltZSk7XG4gIH1cbn1cblxubGV0IHByb2Nlc3NfY291bnRlciA9IC0xO1xuXG5jbGFzcyBQSUQge1xuICBjb25zdHJ1Y3RvcigpIHtcbiAgICBwcm9jZXNzX2NvdW50ZXIgPSBwcm9jZXNzX2NvdW50ZXIgKyAxO1xuICAgIHRoaXMuaWQgPSBwcm9jZXNzX2NvdW50ZXI7XG4gIH1cblxuICB0b1N0cmluZygpIHtcbiAgICByZXR1cm4gXCJQSUQjPDAuXCIgKyB0aGlzLmlkICsgXCIuMD5cIjtcbiAgfVxufVxuXG5jbGFzcyBQcm9jZXNzU3lzdGVtIHtcblxuICBjb25zdHJ1Y3RvcigpIHtcbiAgICB0aGlzLnBpZHMgPSBuZXcgTWFwKCk7XG4gICAgdGhpcy5tYWlsYm94ZXMgPSBuZXcgTWFwKCk7XG4gICAgdGhpcy5uYW1lcyA9IG5ldyBNYXAoKTtcbiAgICB0aGlzLmxpbmtzID0gbmV3IE1hcCgpO1xuXG4gICAgY29uc3QgdGhyb3R0bGUgPSA1OyAvL21zIGJldHdlZW4gc2NoZWR1bGVkIHRhc2tzXG4gICAgdGhpcy5jdXJyZW50X3Byb2Nlc3MgPSBudWxsO1xuICAgIHRoaXMuc2NoZWR1bGVyID0gbmV3IFNjaGVkdWxlcih0aHJvdHRsZSk7XG4gICAgdGhpcy5zdXNwZW5kZWQgPSBuZXcgTWFwKCk7XG5cbiAgICBsZXQgcHJvY2Vzc19zeXN0ZW1fc2NvcGUgPSB0aGlzO1xuICAgIHRoaXMubWFpbl9wcm9jZXNzX3BpZCA9IHRoaXMuc3Bhd24oZnVuY3Rpb24qICgpIHtcbiAgICAgIHdoaWxlICh0cnVlKSB7XG4gICAgICAgIHlpZWxkIHByb2Nlc3Nfc3lzdGVtX3Njb3BlLnNsZWVwKDEwMDAwKTtcbiAgICAgIH1cbiAgICB9KTtcbiAgICB0aGlzLnNldF9jdXJyZW50KHRoaXMubWFpbl9wcm9jZXNzX3BpZCk7XG4gIH1cblxuICBzdGF0aWMgKnJ1bihmdW4sIGFyZ3MsIGNvbnRleHQgPSBudWxsKSB7XG4gICAgaWYgKGZ1bi5jb25zdHJ1Y3Rvci5uYW1lID09PSBcIkdlbmVyYXRvckZ1bmN0aW9uXCIpIHtcbiAgICAgIHJldHVybiB5aWVsZCogZnVuLmFwcGx5KGNvbnRleHQsIGFyZ3MpO1xuICAgIH0gZWxzZSB7XG4gICAgICByZXR1cm4gZnVuLmFwcGx5KGNvbnRleHQsIGFyZ3MpO1xuICAgIH1cbiAgfVxuXG4gIHNwYXduKC4uLmFyZ3MpIHtcbiAgICBpZiAoYXJncy5sZW5ndGggPT09IDEpIHtcbiAgICAgIGxldCBmdW4gPSBhcmdzWzBdO1xuICAgICAgcmV0dXJuIHRoaXMuYWRkX3Byb2MoZnVuLCBbXSwgZmFsc2UpLnBpZDtcbiAgICB9IGVsc2UgaWYgKGFyZ3MubGVuZ3RoID09PSAzKSB7XG4gICAgICBsZXQgbW9kID0gYXJnc1swXTtcbiAgICAgIGxldCBmdW4gPSBhcmdzWzFdO1xuICAgICAgbGV0IHRoZV9hcmdzID0gYXJnc1syXTtcblxuICAgICAgcmV0dXJuIHRoaXMuYWRkX3Byb2MobW9kW2Z1bl0sIHRoZV9hcmdzLCBmYWxzZSkucGlkO1xuICAgIH1cbiAgfVxuXG4gIHNwYXduX2xpbmsoLi4uYXJncykge1xuICAgIGlmIChhcmdzLmxlbmd0aCA9PT0gMSkge1xuICAgICAgbGV0IGZ1biA9IGFyZ3NbMF07XG4gICAgICByZXR1cm4gdGhpcy5hZGRfcHJvYyhmdW4sIFtdLCB0cnVlKS5waWQ7XG4gICAgfSBlbHNlIGlmIChhcmdzLmxlbmd0aCA9PT0gMykge1xuICAgICAgbGV0IG1vZCA9IGFyZ3NbMF07XG4gICAgICBsZXQgZnVuID0gYXJnc1sxXTtcbiAgICAgIGxldCB0aGVfYXJncyA9IGFyZ3NbMl07XG5cbiAgICAgIHJldHVybiB0aGlzLmFkZF9wcm9jKG1vZFtmdW5dLCB0aGVfYXJncywgdHJ1ZSkucGlkO1xuICAgIH1cbiAgfVxuXG4gIGxpbmsocGlkKSB7XG4gICAgdGhpcy5saW5rcy5nZXQodGhpcy5waWQoKSkuYWRkKHBpZCk7XG4gICAgdGhpcy5saW5rcy5nZXQocGlkKS5hZGQodGhpcy5waWQoKSk7XG4gIH1cblxuICB1bmxpbmsocGlkKSB7XG4gICAgdGhpcy5saW5rcy5nZXQodGhpcy5waWQoKSkuZGVsZXRlKHBpZCk7XG4gICAgdGhpcy5saW5rcy5nZXQocGlkKS5kZWxldGUodGhpcy5waWQoKSk7XG4gIH1cblxuICBzZXRfY3VycmVudChpZCkge1xuICAgIGxldCBwaWQgPSB0aGlzLnBpZG9mKGlkKTtcbiAgICBpZiAocGlkICE9PSBudWxsKSB7XG4gICAgICB0aGlzLmN1cnJlbnRfcHJvY2VzcyA9IHRoaXMucGlkcy5nZXQocGlkKTtcbiAgICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLnN0YXR1cyA9IFN0YXRlcy5SVU5OSU5HO1xuICAgIH1cbiAgfVxuXG4gIGFkZF9wcm9jKGZ1biwgYXJncywgbGlua2VkKSB7XG4gICAgbGV0IG5ld3BpZCA9IG5ldyBQSUQoKTtcbiAgICBsZXQgbWFpbGJveCA9IG5ldyBNYWlsYm94KCk7XG4gICAgbGV0IG5ld3Byb2MgPSBuZXcgUHJvY2VzcyhuZXdwaWQsIGZ1biwgYXJncywgbWFpbGJveCwgdGhpcyk7XG5cbiAgICB0aGlzLnBpZHMuc2V0KG5ld3BpZCwgbmV3cHJvYyk7XG4gICAgdGhpcy5tYWlsYm94ZXMuc2V0KG5ld3BpZCwgbWFpbGJveCk7XG4gICAgdGhpcy5saW5rcy5zZXQobmV3cGlkLCBuZXcgU2V0KCkpO1xuXG4gICAgaWYgKGxpbmtlZCkge1xuICAgICAgdGhpcy5saW5rKG5ld3BpZCk7XG4gICAgfVxuXG4gICAgbmV3cHJvYy5zdGFydCgpO1xuICAgIHJldHVybiBuZXdwcm9jO1xuICB9XG5cbiAgcmVtb3ZlX3Byb2MocGlkLCBleGl0cmVhc29uKSB7XG4gICAgdGhpcy5waWRzLmRlbGV0ZShwaWQpO1xuICAgIHRoaXMudW5yZWdpc3RlcihwaWQpO1xuICAgIHRoaXMuc2NoZWR1bGVyLnJlbW92ZVBpZChwaWQpO1xuXG4gICAgaWYgKHRoaXMubGlua3MuaGFzKHBpZCkpIHtcbiAgICAgIGZvciAobGV0IGxpbmtwaWQgb2YgdGhpcy5saW5rcy5nZXQocGlkKSkge1xuICAgICAgICB0aGlzLmV4aXQobGlua3BpZCwgZXhpdHJlYXNvbik7XG4gICAgICAgIHRoaXMubGlua3MuZ2V0KGxpbmtwaWQpLmRlbGV0ZShwaWQpO1xuICAgICAgfVxuXG4gICAgICB0aGlzLmxpbmtzLmRlbGV0ZShwaWQpO1xuICAgIH1cbiAgfVxuXG4gIHJlZ2lzdGVyKG5hbWUsIHBpZCkge1xuICAgIGlmICghdGhpcy5uYW1lcy5oYXMobmFtZSkpIHtcbiAgICAgIHRoaXMubmFtZXMuc2V0KG5hbWUsIHBpZCk7XG4gICAgfSBlbHNlIHtcbiAgICAgIHRocm93IG5ldyBFcnJvcihcIk5hbWUgaXMgYWxyZWFkeSByZWdpc3RlcmVkIHRvIGFub3RoZXIgcHJvY2Vzc1wiKTtcbiAgICB9XG4gIH1cblxuICByZWdpc3RlcmVkKG5hbWUpIHtcbiAgICByZXR1cm4gdGhpcy5uYW1lcy5oYXMobmFtZSkgPyB0aGlzLm5hbWVzLmdldChuYW1lKSA6IG51bGw7XG4gIH1cblxuICB1bnJlZ2lzdGVyKHBpZCkge1xuICAgIGZvciAobGV0IG5hbWUgb2YgdGhpcy5uYW1lcy5rZXlzKCkpIHtcbiAgICAgIGlmICh0aGlzLm5hbWVzLmhhcyhuYW1lKSAmJiB0aGlzLm5hbWVzLmdldChuYW1lKSA9PT0gcGlkKSB7XG4gICAgICAgIHRoaXMubmFtZXMuZGVsZXRlKG5hbWUpO1xuICAgICAgfVxuICAgIH1cbiAgfVxuXG4gIHBpZCgpIHtcbiAgICByZXR1cm4gdGhpcy5jdXJyZW50X3Byb2Nlc3MucGlkO1xuICB9XG5cbiAgcGlkb2YoaWQpIHtcbiAgICBpZiAoaWQgaW5zdGFuY2VvZiBQSUQpIHtcbiAgICAgIHJldHVybiB0aGlzLnBpZHMuaGFzKGlkKSA/IGlkIDogbnVsbDtcbiAgICB9IGVsc2UgaWYgKGlkIGluc3RhbmNlb2YgUHJvY2Vzcykge1xuICAgICAgcmV0dXJuIGlkLnBpZDtcbiAgICB9IGVsc2Uge1xuICAgICAgbGV0IHBpZCA9IHRoaXMucmVnaXN0ZXJlZChpZCk7XG4gICAgICBpZiAocGlkID09PSBudWxsKSB0aHJvdyBcIlByb2Nlc3MgbmFtZSBub3QgcmVnaXN0ZXJlZDogXCIgKyBpZCArIFwiIChcIiArIHR5cGVvZiBpZCArIFwiKVwiO1xuICAgICAgcmV0dXJuIHBpZDtcbiAgICB9XG4gIH1cblxuICBzZW5kKGlkLCBtc2cpIHtcbiAgICBjb25zdCBwaWQgPSB0aGlzLnBpZG9mKGlkKTtcblxuICAgIGlmIChwaWQpIHtcbiAgICAgIHRoaXMubWFpbGJveGVzLmdldChwaWQpLmRlbGl2ZXIobXNnKTtcblxuICAgICAgaWYgKHRoaXMuc3VzcGVuZGVkLmhhcyhwaWQpKSB7XG4gICAgICAgIGxldCBmdW4gPSB0aGlzLnN1c3BlbmRlZC5nZXQocGlkKTtcbiAgICAgICAgdGhpcy5zdXNwZW5kZWQuZGVsZXRlKHBpZCk7XG4gICAgICAgIHRoaXMuc2NoZWR1bGUoZnVuKTtcbiAgICAgIH1cbiAgICB9XG5cbiAgICByZXR1cm4gbXNnO1xuICB9XG5cbiAgcmVjZWl2ZShmdW4sIHRpbWVvdXQgPSAwLCB0aW1lb3V0Rm4gPSAoKSA9PiB0cnVlKSB7XG4gICAgbGV0IERhdGVUaW1lb3V0ID0gbnVsbDtcblxuICAgIGlmICh0aW1lb3V0ID09PSAwIHx8IHRpbWVvdXQgPT09IEluZmluaXR5KSB7XG4gICAgICBEYXRlVGltZW91dCA9IG51bGw7XG4gICAgfSBlbHNlIHtcbiAgICAgIERhdGVUaW1lb3V0ID0gRGF0ZS5ub3coKSArIHRpbWVvdXQ7XG4gICAgfVxuXG4gICAgcmV0dXJuIFtTdGF0ZXMuUkVDRUlWRSwgZnVuLCBEYXRlVGltZW91dCwgdGltZW91dEZuXTtcbiAgfVxuXG4gIHNsZWVwKGR1cmF0aW9uKSB7XG4gICAgcmV0dXJuIFtTdGF0ZXMuU0xFRVAsIGR1cmF0aW9uXTtcbiAgfVxuXG4gIHN1c3BlbmQoZnVuKSB7XG4gICAgdGhpcy5jdXJyZW50X3Byb2Nlc3Muc3RhdHVzID0gU3RhdGVzLlNVU1BFTkRFRDtcbiAgICB0aGlzLnN1c3BlbmRlZC5zZXQodGhpcy5jdXJyZW50X3Byb2Nlc3MucGlkLCBmdW4pO1xuICB9XG5cbiAgZGVsYXkoZnVuLCB0aW1lKSB7XG4gICAgdGhpcy5jdXJyZW50X3Byb2Nlc3Muc3RhdHVzID0gU3RhdGVzLlNMRUVQSU5HO1xuICAgIHRoaXMuc2NoZWR1bGVyLnNjaGVkdWxlRnV0dXJlKHRoaXMuY3VycmVudF9wcm9jZXNzLnBpZCwgdGltZSwgZnVuKTtcbiAgfVxuXG4gIHNjaGVkdWxlKGZ1biwgcGlkKSB7XG4gICAgY29uc3QgdGhlX3BpZCA9IHBpZCAhPSBudWxsID8gcGlkIDogdGhpcy5jdXJyZW50X3Byb2Nlc3MucGlkO1xuICAgIHRoaXMuc2NoZWR1bGVyLnNjaGVkdWxlKHRoZV9waWQsIGZ1bik7XG4gIH1cblxuICBleGl0KG9uZSwgdHdvKSB7XG4gICAgaWYgKHR3bykge1xuICAgICAgbGV0IHBpZCA9IG9uZTtcbiAgICAgIGxldCByZWFzb24gPSB0d287XG5cbiAgICAgIGxldCBwcm9jZXNzID0gdGhpcy5waWRzLmdldCh0aGlzLnBpZG9mKHBpZCkpO1xuICAgICAgaWYgKHByb2Nlc3MgJiYgcHJvY2Vzcy5pc190cmFwcGluZ19leGl0cygpIHx8IHJlYXNvbiA9PT0gU3RhdGVzLktJTEwgfHwgcmVhc29uID09PSBTdGF0ZXMuTk9STUFMKSB7XG4gICAgICAgIHRoaXMubWFpbGJveGVzLmdldChwcm9jZXNzLnBpZCkuZGVsaXZlcihbU3RhdGVzLkVYSVQsIHRoaXMucGlkKCksIHJlYXNvbl0pO1xuICAgICAgfSBlbHNlIHtcbiAgICAgICAgcHJvY2Vzcy5zaWduYWwocmVhc29uKTtcbiAgICAgIH1cbiAgICB9IGVsc2Uge1xuICAgICAgbGV0IHJlYXNvbiA9IG9uZTtcbiAgICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLnNpZ25hbChyZWFzb24pO1xuICAgIH1cbiAgfVxuXG4gIGVycm9yKHJlYXNvbikge1xuICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLnNpZ25hbChyZWFzb24pO1xuICB9XG5cbiAgcHJvY2Vzc19mbGFnKGZsYWcsIHZhbHVlKSB7XG4gICAgdGhpcy5jdXJyZW50X3Byb2Nlc3MucHJvY2Vzc19mbGFnKGZsYWcsIHZhbHVlKTtcbiAgfVxuXG4gIHB1dChrZXksIHZhbHVlKSB7XG4gICAgdGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdFtrZXldID0gdmFsdWU7XG4gIH1cblxuICBnZXQoa2V5KSB7XG4gICAgaWYgKGtleSAhPSBudWxsKSB7XG4gICAgICByZXR1cm4gdGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdFtrZXldO1xuICAgIH0gZWxzZSB7XG4gICAgICByZXR1cm4gdGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdDtcbiAgICB9XG4gIH1cblxuICBnZXRfa2V5cygpIHtcbiAgICByZXR1cm4gT2JqZWN0LmtleXModGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdCk7XG4gIH1cblxuICBlcmFzZShrZXkpIHtcbiAgICBpZiAoa2V5ICE9IG51bGwpIHtcbiAgICAgIGRlbGV0ZSB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0W2tleV07XG4gICAgfSBlbHNlIHtcbiAgICAgIHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3QgPSB7fTtcbiAgICB9XG4gIH1cbn1cblxuZnVuY3Rpb24gc3RhcnQobW9kdWxlLCBhcmdzKSB7XG4gIHJldHVybiBbU3ltYm9sLmZvcihcIm9rXCIpLCBzZWxmLnN5c3RlbS5zcGF3bihzdGFydF9wcm9jZXNzKG1vZHVsZSwgYXJncykpXTtcbn1cblxuZnVuY3Rpb24gc3RhcnRfbGluayhtb2R1bGUsIGFyZ3MpIHtcbiAgcmV0dXJuIFtTeW1ib2wuZm9yKFwib2tcIiksIHNlbGYuc3lzdGVtLnNwYXduX2xpbmsoc3RhcnRfcHJvY2Vzcyhtb2R1bGUsIGFyZ3MpKV07XG59XG5cbmZ1bmN0aW9uIHN0YXJ0X3Byb2Nlc3MobW9kdWxlLCBhcmdzKSB7XG4gIHJldHVybiBmdW5jdGlvbiogKCkge1xuICAgIGxldCBbb2ssIHN0YXRlXSA9IG1vZHVsZS5pbml0LmFwcGx5KG51bGwsIFthcmdzXSk7XG4gICAgeWllbGQgc2VsZi5zeXN0ZW0ucHV0KFwic3RhdGVcIiwgc3RhdGUpO1xuXG4gICAgdHJ5IHtcbiAgICAgIHdoaWxlICh0cnVlKSB7XG4gICAgICAgIHlpZWxkIHNlbGYuc3lzdGVtLnJlY2VpdmUoZnVuY3Rpb24gKGFyZ3MpIHtcbiAgICAgICAgICBsZXQgY29tbWFuZCA9IGFyZ3NbMF07XG5cbiAgICAgICAgICBzd2l0Y2ggKGNvbW1hbmQpIHtcbiAgICAgICAgICAgIGNhc2UgXCJjYWxsXCI6XG4gICAgICAgICAgICAgIHZhciByZXF1ZXN0ID0gYXJnc1sxXTtcbiAgICAgICAgICAgICAgdmFyIHNlbmRlciA9IGFyZ3NbMl07XG5cbiAgICAgICAgICAgICAgdmFyIFtyZXBseSwgcmVzcG9uc2UsIG5ld19zdGF0ZV0gPSBtb2R1bGUuaGFuZGxlX2NhbGwocmVxdWVzdCwgc2VuZGVyLCBzZWxmLnN5c3RlbS5nZXQoXCJzdGF0ZVwiKSk7XG4gICAgICAgICAgICAgIHNlbGYuc3lzdGVtLnB1dChcInN0YXRlXCIsIG5ld19zdGF0ZSk7XG5cbiAgICAgICAgICAgICAgc2VsZi5zeXN0ZW0uc2VuZChzZW5kZXIsIHJlc3BvbnNlKTtcbiAgICAgICAgICAgICAgYnJlYWs7XG5cbiAgICAgICAgICAgIGNhc2UgXCJjYXN0XCI6XG4gICAgICAgICAgICAgIHZhciByZXF1ZXN0ID0gYXJnc1sxXTtcbiAgICAgICAgICAgICAgdmFyIHNlbmRlciA9IGFyZ3NbMl07XG5cbiAgICAgICAgICAgICAgdmFyIFtyZXBseSwgbmV3X3N0YXRlXSA9IG1vZHVsZS5oYW5kbGVfY2FzdChyZXF1ZXN0LCBzZWxmLnN5c3RlbS5nZXQoXCJzdGF0ZVwiKSk7XG5cbiAgICAgICAgICAgICAgc2VsZi5zeXN0ZW0ucHV0KFwic3RhdGVcIiwgbmV3X3N0YXRlKTtcbiAgICAgICAgICAgICAgc2VsZi5zeXN0ZW0uc2VuZChhcmdzWzJdLCBTeW1ib2wuZm9yKFwib2tcIikpO1xuXG4gICAgICAgICAgICAgIGJyZWFrO1xuXG4gICAgICAgICAgICBjYXNlIFwic3RvcFwiOlxuICAgICAgICAgICAgICB0aHJvdyBcInN0b3BcIjtcbiAgICAgICAgICB9XG4gICAgICAgIH0pO1xuICAgICAgfVxuICAgIH0gY2F0Y2ggKGUpIHtcbiAgICAgIGlmIChlICE9PSBcInN0b3BcIikge1xuICAgICAgICB0aHJvdyBlO1xuICAgICAgfVxuICAgIH1cbiAgfTtcbn1cblxuZnVuY3Rpb24qIGNhbGwoc2VydmVyLCByZXF1ZXN0KSB7XG4gIHNlbGYuc3lzdGVtLnNlbmQoc2VydmVyLCBbXCJjYWxsXCIsIHJlcXVlc3QsIHNlbGYuc3lzdGVtLnBpZCgpXSk7XG5cbiAgcmV0dXJuIHlpZWxkIHNlbGYuc3lzdGVtLnJlY2VpdmUoZnVuY3Rpb24gKGFyZ3MpIHtcbiAgICByZXR1cm4gYXJncztcbiAgfSk7XG59XG5cbmZ1bmN0aW9uKiBjYXN0KHNlcnZlciwgcmVxdWVzdCkge1xuICBzZWxmLnN5c3RlbS5zZW5kKHNlcnZlciwgW1wiY2FzdFwiLCByZXF1ZXN0LCBzZWxmLnN5c3RlbS5waWQoKV0pO1xuXG4gIHJldHVybiB5aWVsZCBzZWxmLnN5c3RlbS5yZWNlaXZlKGZ1bmN0aW9uIChhcmdzKSB7XG4gICAgcmV0dXJuIGFyZ3M7XG4gIH0pO1xufVxuXG5mdW5jdGlvbiBzdG9wKHNlcnZlcikge1xuICBzZWxmLnN5c3RlbS5zZW5kKHNlcnZlciwgW1wic3RvcFwiXSk7XG59XG5cbnZhciBnZW5fc2VydmVyID0geyBzdGFydCwgc3RhcnRfbGluaywgY2FsbCwgY2FzdCwgc3RvcCB9O1xuXG5leHBvcnQgeyBQcm9jZXNzU3lzdGVtLCBnZW5fc2VydmVyIGFzIEdlblNlcnZlciB9OyJdLCJzb3VyY2VSb290IjoiL3NvdXJjZS8ifQ==
