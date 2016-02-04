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
      let iter = this.queues.entries();
      let next = iter.next();
      this.invokeLater(this.do_run(next, iter, this.reductions_per_process));
    }
  }, {
    key: "do_run",
    value: function do_run(entry, queueIterator, reductions) {
      if (entry.done == true) {
        let iter = this.queues.entries();
        let next = iter.next();
        this.invokeLater(() => this.do_run(next, iter, this.reductions_per_process));
      } else if (this.isRunning) {
        this.invokeLater(this.do_run(entry, queueIterator, reductions));
      } else if (reductions == 0 || !entry.value[1] || entry.value[1].empty()) {
        let next = queueIterator.next();
        this.invokeLater(this.do_run(next, queueIterator, this.reductions_per_process));
      } else {
        let queue = entry.value[1];
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

        this.invokeLater(this.do_run(entry, queueIterator, reductions - 1));
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
//# sourceMappingURL=data:application/json;base64,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Q0FBQyxNQUFNLEVBQUUsQ0FBQyxNQUFNLEVBQUUsT0FBTyxFQUFFLElBQUksQ0FBQyxNQUFNLENBQUMsR0FBRyxFQUFFLENBQUMsQ0FBQyxDQUFDOztBQUUvRCxTQUFPLE1BQU0sSUFBSSxDQUFDLE1BQU0sQ0FBQyxPQUFPLENBQUMsVUFBVSxJQUFJLEVBQUU7QUFDL0MsV0FBTyxJQUFJLENBQUM7R0FDYixDQUFDLENBQUM7Q0FDSjs7QUFFRCxVQUFVLElBQUksQ0FBQyxNQUFNLEVBQUUsT0FBTyxFQUFFO0FBQzlCLE1BQUksQ0FBQyxNQUFNLENBQUMsSUFBSSxDQUFDLE1BQU0sRUFBRSxDQUFDLE1BQU0sRUFBRSxPQUFPLEVBQUUsSUFBSSxDQUFDLE1BQU0sQ0FBQyxHQUFHLEVBQUUsQ0FBQyxDQUFDLENBQUM7O0FBRS9ELFNBQU8sTUFBTSxJQUFJLENBQUMsTUFBTSxDQUFDLE9BQU8sQ0FBQyxVQUFVLElBQUksRUFBRTtBQUMvQyxXQUFPLElBQUksQ0FBQztHQUNiLENBQUMsQ0FBQztDQUNKOztBQUVELFNBQVMsSUFBSSxDQUFDLE1BQU0sRUFBRTtBQUNwQixNQUFJLENBQUMsTUFBTSxDQUFDLElBQUksQ0FBQyxNQUFNLEVBQUUsQ0FBQyxNQUFNLENBQUMsQ0FBQyxDQUFDO0NBQ3BDOztBQUVELElBQUksVUFBVSxHQUFHLEVBQUUsS0FBSyxFQUFMLEtBQUssRUFBRSxVQUFVLEVBQVYsVUFBVSxFQUFFLElBQUksRUFBSixJQUFJLEVBQUUsSUFBSSxFQUFKLElBQUksRUFBRSxJQUFJLEVBQUosSUFBSSxFQUFFLENBQUM7O1FBRWhELGFBQWEsR0FBYixhQUFhO1FBQWdCLFNBQVMsR0FBdkIsVUFBVSIsImZpbGUiOiJpbmRleC5qcyIsInNvdXJjZXNDb250ZW50IjpbImNsYXNzIE1haWxib3gge1xuXG4gIGNvbnN0cnVjdG9yKCkge1xuICAgIHRoaXMubWVzc2FnZXMgPSBbXTtcbiAgfVxuXG4gIGRlbGl2ZXIobWVzc2FnZSkge1xuICAgIHRoaXMubWVzc2FnZXMucHVzaChtZXNzYWdlKTtcbiAgICByZXR1cm4gbWVzc2FnZTtcbiAgfVxuXG4gIGdldCgpIHtcbiAgICByZXR1cm4gdGhpcy5tZXNzYWdlcztcbiAgfVxuXG4gIGlzRW1wdHkoKSB7XG4gICAgcmV0dXJuIHRoaXMubWVzc2FnZXMubGVuZ3RoID09PSAwO1xuICB9XG5cbiAgcmVtb3ZlQXQoaW5kZXgpIHtcbiAgICB0aGlzLm1lc3NhZ2VzLnNwbGljZShpbmRleCwgMSk7XG4gIH1cbn1cblxudmFyIFN0YXRlcyA9IHtcbiAgTk9STUFMOiBTeW1ib2wuZm9yKFwibm9ybWFsXCIpLFxuICBLSUxMOiBTeW1ib2wuZm9yKFwia2lsbFwiKSxcbiAgU1VTUEVORDogU3ltYm9sLmZvcihcInN1c3BlbmRcIiksXG4gIENPTlRJTlVFOiBTeW1ib2wuZm9yKFwiY29udGludWVcIiksXG4gIFJFQ0VJVkU6IFN5bWJvbC5mb3IoXCJyZWNlaXZlXCIpLFxuICBTRU5EOiBTeW1ib2wuZm9yKFwic2VuZFwiKSxcbiAgU0xFRVBJTkc6IFN5bWJvbC5mb3IoXCJzbGVlcGluZ1wiKSxcbiAgUlVOTklORzogU3ltYm9sLmZvcihcInJ1bm5pbmdcIiksXG4gIFNVU1BFTkRFRDogU3ltYm9sLmZvcihcInN1c3BlbmRlZFwiKSxcbiAgU1RPUFBFRDogU3ltYm9sLmZvcihcInN0b3BwZWRcIiksXG4gIFNMRUVQOiBTeW1ib2wuZm9yKFwic2xlZXBcIiksXG4gIEVYSVQ6IFN5bWJvbC5mb3IoXCJleGl0XCIpLFxuICBOT01BVENIOiBTeW1ib2wuZm9yKFwibm9fbWF0Y2hcIilcbn07XG5cbmZ1bmN0aW9uIGlzX3NsZWVwKHZhbHVlKSB7XG4gIHJldHVybiBBcnJheS5pc0FycmF5KHZhbHVlKSAmJiB2YWx1ZVswXSA9PT0gU3RhdGVzLlNMRUVQO1xufVxuXG5mdW5jdGlvbiBpc19yZWNlaXZlKHZhbHVlKSB7XG4gIHJldHVybiBBcnJheS5pc0FycmF5KHZhbHVlKSAmJiB2YWx1ZVswXSA9PT0gU3RhdGVzLlJFQ0VJVkU7XG59XG5cbmZ1bmN0aW9uIHJlY2VpdmVfdGltZWRfb3V0KHZhbHVlKSB7XG4gIHJldHVybiB2YWx1ZVsyXSAhPSBudWxsICYmIHZhbHVlWzJdIDwgRGF0ZS5ub3coKTtcbn1cblxuY2xhc3MgUHJvY2VzcyB7XG5cbiAgY29uc3RydWN0b3IocGlkLCBmdW5jLCBhcmdzLCBtYWlsYm94LCBzeXN0ZW0pIHtcbiAgICB0aGlzLnBpZCA9IHBpZDtcbiAgICB0aGlzLmZ1bmMgPSBmdW5jO1xuICAgIHRoaXMuYXJncyA9IGFyZ3M7XG4gICAgdGhpcy5tYWlsYm94ID0gbWFpbGJveDtcbiAgICB0aGlzLnN5c3RlbSA9IHN5c3RlbTtcbiAgICB0aGlzLnN0YXR1cyA9IFN0YXRlcy5TVE9QUEVEO1xuICAgIHRoaXMuZGljdCA9IHt9O1xuICAgIHRoaXMuZmxhZ3MgPSB7fTtcbiAgfVxuXG4gIHN0YXJ0KCkge1xuICAgIGNvbnN0IGZ1bmN0aW9uX3Njb3BlID0gdGhpcztcbiAgICBsZXQgbWFjaGluZSA9IHRoaXMubWFpbigpO1xuXG4gICAgdGhpcy5zeXN0ZW0uc2NoZWR1bGUoZnVuY3Rpb24gKCkge1xuICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgbWFjaGluZS5uZXh0KCkpO1xuICAgIH0sIHRoaXMucGlkKTtcbiAgfVxuXG4gICptYWluKCkge1xuICAgIGxldCByZXR2YWwgPSBTdGF0ZXMuTk9STUFMO1xuXG4gICAgdHJ5IHtcbiAgICAgIHlpZWxkKiB0aGlzLmZ1bmMuYXBwbHkobnVsbCwgdGhpcy5hcmdzKTtcbiAgICB9IGNhdGNoIChlKSB7XG4gICAgICBjb25zb2xlLmVycm9yKGUpO1xuICAgICAgcmV0dmFsID0gZTtcbiAgICB9XG5cbiAgICB0aGlzLnN5c3RlbS5leGl0KHJldHZhbCk7XG4gIH1cblxuICBwcm9jZXNzX2ZsYWcoZmxhZywgdmFsdWUpIHtcbiAgICB0aGlzLmZsYWdzW2ZsYWddID0gdmFsdWU7XG4gIH1cblxuICBpc190cmFwcGluZ19leGl0cygpIHtcbiAgICByZXR1cm4gdGhpcy5mbGFnc1tTeW1ib2wuZm9yKFwidHJhcF9leGl0XCIpXSAmJiB0aGlzLmZsYWdzW1N5bWJvbC5mb3IoXCJ0cmFwX2V4aXRcIildID09IHRydWU7XG4gIH1cblxuICBzaWduYWwocmVhc29uKSB7XG4gICAgaWYgKHJlYXNvbiAhPT0gU3RhdGVzLk5PUk1BTCkge1xuICAgICAgY29uc29sZS5lcnJvcihyZWFzb24pO1xuICAgIH1cblxuICAgIHRoaXMuc3lzdGVtLnJlbW92ZV9wcm9jKHRoaXMucGlkLCByZWFzb24pO1xuICB9XG5cbiAgcmVjZWl2ZShmdW4pIHtcbiAgICBsZXQgdmFsdWUgPSBTdGF0ZXMuTk9NQVRDSDtcbiAgICBsZXQgbWVzc2FnZXMgPSB0aGlzLm1haWxib3guZ2V0KCk7XG5cbiAgICBmb3IgKGxldCBpID0gMDsgaSA8IG1lc3NhZ2VzLmxlbmd0aDsgaSsrKSB7XG4gICAgICB0cnkge1xuICAgICAgICB2YWx1ZSA9IGZ1bihtZXNzYWdlc1tpXSk7XG4gICAgICAgIGlmICh2YWx1ZSAhPT0gU3RhdGVzLk5PTUFUQ0gpIHtcbiAgICAgICAgICB0aGlzLm1haWxib3gucmVtb3ZlQXQoaSk7XG4gICAgICAgICAgYnJlYWs7XG4gICAgICAgIH1cbiAgICAgIH0gY2F0Y2ggKGUpIHtcbiAgICAgICAgdGhpcy5leGl0KGUpO1xuICAgICAgfVxuICAgIH1cblxuICAgIHJldHVybiB2YWx1ZTtcbiAgfVxuXG4gIHJ1bihtYWNoaW5lLCBzdGVwKSB7XG4gICAgY29uc3QgZnVuY3Rpb25fc2NvcGUgPSB0aGlzO1xuXG4gICAgaWYgKCFzdGVwLmRvbmUpIHtcbiAgICAgIGxldCB2YWx1ZSA9IHN0ZXAudmFsdWU7XG5cbiAgICAgIGlmIChpc19zbGVlcCh2YWx1ZSkpIHtcblxuICAgICAgICB0aGlzLnN5c3RlbS5kZWxheShmdW5jdGlvbiAoKSB7XG4gICAgICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICAgICAgZnVuY3Rpb25fc2NvcGUucnVuKG1hY2hpbmUsIG1hY2hpbmUubmV4dCgpKTtcbiAgICAgICAgfSwgdmFsdWVbMV0pO1xuICAgICAgfSBlbHNlIGlmIChpc19yZWNlaXZlKHZhbHVlKSAmJiByZWNlaXZlX3RpbWVkX291dCh2YWx1ZSkpIHtcblxuICAgICAgICBsZXQgcmVzdWx0ID0gdmFsdWVbM10oKTtcblxuICAgICAgICB0aGlzLnN5c3RlbS5zY2hlZHVsZShmdW5jdGlvbiAoKSB7XG4gICAgICAgICAgZnVuY3Rpb25fc2NvcGUuc3lzdGVtLnNldF9jdXJyZW50KGZ1bmN0aW9uX3Njb3BlLnBpZCk7XG4gICAgICAgICAgZnVuY3Rpb25fc2NvcGUucnVuKG1hY2hpbmUsIG1hY2hpbmUubmV4dChyZXN1bHQpKTtcbiAgICAgICAgfSk7XG4gICAgICB9IGVsc2UgaWYgKGlzX3JlY2VpdmUodmFsdWUpKSB7XG5cbiAgICAgICAgbGV0IHJlc3VsdCA9IGZ1bmN0aW9uX3Njb3BlLnJlY2VpdmUodmFsdWVbMV0pO1xuXG4gICAgICAgIGlmIChyZXN1bHQgPT09IFN0YXRlcy5OT01BVENIKSB7XG4gICAgICAgICAgdGhpcy5zeXN0ZW0uc3VzcGVuZChmdW5jdGlvbiAoKSB7XG4gICAgICAgICAgICBmdW5jdGlvbl9zY29wZS5zeXN0ZW0uc2V0X2N1cnJlbnQoZnVuY3Rpb25fc2NvcGUucGlkKTtcbiAgICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnJ1bihtYWNoaW5lLCBzdGVwKTtcbiAgICAgICAgICB9KTtcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICB0aGlzLnN5c3RlbS5zY2hlZHVsZShmdW5jdGlvbiAoKSB7XG4gICAgICAgICAgICBmdW5jdGlvbl9zY29wZS5zeXN0ZW0uc2V0X2N1cnJlbnQoZnVuY3Rpb25fc2NvcGUucGlkKTtcbiAgICAgICAgICAgIGZ1bmN0aW9uX3Njb3BlLnJ1bihtYWNoaW5lLCBtYWNoaW5lLm5leHQocmVzdWx0KSk7XG4gICAgICAgICAgfSk7XG4gICAgICAgIH1cbiAgICAgIH0gZWxzZSB7XG4gICAgICAgIHRoaXMuc3lzdGVtLnNjaGVkdWxlKGZ1bmN0aW9uICgpIHtcbiAgICAgICAgICBmdW5jdGlvbl9zY29wZS5zeXN0ZW0uc2V0X2N1cnJlbnQoZnVuY3Rpb25fc2NvcGUucGlkKTtcbiAgICAgICAgICBmdW5jdGlvbl9zY29wZS5ydW4obWFjaGluZSwgbWFjaGluZS5uZXh0KHZhbHVlKSk7XG4gICAgICAgIH0pO1xuICAgICAgfVxuICAgIH1cbiAgfVxufVxuXG5jbGFzcyBQcm9jZXNzUXVldWUge1xuICBjb25zdHJ1Y3RvcihwaWQpIHtcbiAgICB0aGlzLnBpZCA9IHBpZDtcbiAgICB0aGlzLnRhc2tzID0gW107XG4gIH1cblxuICBlbXB0eSgpIHtcbiAgICByZXR1cm4gdGhpcy50YXNrcy5sZW5ndGggPT09IDA7XG4gIH1cblxuICBhZGQodGFzaykge1xuICAgIHRoaXMudGFza3MucHVzaCh0YXNrKTtcbiAgfVxuXG4gIG5leHQoKSB7XG4gICAgcmV0dXJuIHRoaXMudGFza3Muc2hpZnQoKTtcbiAgfVxufVxuXG5jbGFzcyBTY2hlZHVsZXIge1xuICBjb25zdHJ1Y3Rvcih0aHJvdHRsZSA9IDAsIHJlZHVjdGlvbnNfcGVyX3Byb2Nlc3MgPSA4KSB7XG4gICAgdGhpcy5pc1J1bm5pbmcgPSBmYWxzZTtcbiAgICB0aGlzLmludm9rZUxhdGVyID0gZnVuY3Rpb24gKGNhbGxiYWNrKSB7XG4gICAgICBzZXRUaW1lb3V0KGNhbGxiYWNrLCB0aHJvdHRsZSk7XG4gICAgfTtcblxuICAgIC8vIEluIG91ciBjYXNlIGEgcmVkdWN0aW9uIGlzIGVxdWFsIHRvIGEgdGFzayBjYWxsXG4gICAgLy8gQ29udHJvbHMgaG93IG1hbnkgdGFza3MgYXJlIGNhbGxlZCBhdCBhIHRpbWUgcGVyIHByb2Nlc3NcbiAgICB0aGlzLnJlZHVjdGlvbnNfcGVyX3Byb2Nlc3MgPSByZWR1Y3Rpb25zX3Blcl9wcm9jZXNzO1xuICAgIHRoaXMucXVldWVzID0gbmV3IE1hcCgpO1xuICAgIHRoaXMucnVuKCk7XG4gIH1cblxuICBhZGRUb1F1ZXVlKHBpZCwgdGFzaykge1xuICAgIGlmICghdGhpcy5xdWV1ZXMuaGFzKHBpZCkpIHtcbiAgICAgIHRoaXMucXVldWVzLnNldChwaWQsIG5ldyBQcm9jZXNzUXVldWUocGlkKSk7XG4gICAgfVxuXG4gICAgdGhpcy5xdWV1ZXMuZ2V0KHBpZCkuYWRkKHRhc2spO1xuICB9XG5cbiAgcmVtb3ZlUGlkKHBpZCkge1xuICAgIHRoaXMuaXNSdW5uaW5nID0gdHJ1ZTtcblxuICAgIHRoaXMucXVldWVzLmRlbGV0ZShwaWQpO1xuXG4gICAgdGhpcy5pc1J1bm5pbmcgPSBmYWxzZTtcbiAgfVxuXG4gIHJ1bigpIHtcbiAgICBsZXQgaXRlciA9IHRoaXMucXVldWVzLmVudHJpZXMoKTtcbiAgICBsZXQgbmV4dCA9IGl0ZXIubmV4dCgpO1xuICAgIHRoaXMuaW52b2tlTGF0ZXIodGhpcy5kb19ydW4obmV4dCwgaXRlciwgdGhpcy5yZWR1Y3Rpb25zX3Blcl9wcm9jZXNzKSk7XG4gIH1cblxuICBkb19ydW4oZW50cnksIHF1ZXVlSXRlcmF0b3IsIHJlZHVjdGlvbnMpIHtcbiAgICBpZiAoZW50cnkuZG9uZSA9PSB0cnVlKSB7XG4gICAgICBsZXQgaXRlciA9IHRoaXMucXVldWVzLmVudHJpZXMoKTtcbiAgICAgIGxldCBuZXh0ID0gaXRlci5uZXh0KCk7XG4gICAgICB0aGlzLmludm9rZUxhdGVyKCgpID0+IHRoaXMuZG9fcnVuKG5leHQsIGl0ZXIsIHRoaXMucmVkdWN0aW9uc19wZXJfcHJvY2VzcykpO1xuICAgIH0gZWxzZSBpZiAodGhpcy5pc1J1bm5pbmcpIHtcbiAgICAgIHRoaXMuaW52b2tlTGF0ZXIodGhpcy5kb19ydW4oZW50cnksIHF1ZXVlSXRlcmF0b3IsIHJlZHVjdGlvbnMpKTtcbiAgICB9IGVsc2UgaWYgKHJlZHVjdGlvbnMgPT0gMCB8fCAhZW50cnkudmFsdWVbMV0gfHwgZW50cnkudmFsdWVbMV0uZW1wdHkoKSkge1xuICAgICAgbGV0IG5leHQgPSBxdWV1ZUl0ZXJhdG9yLm5leHQoKTtcbiAgICAgIHRoaXMuaW52b2tlTGF0ZXIodGhpcy5kb19ydW4obmV4dCwgcXVldWVJdGVyYXRvciwgdGhpcy5yZWR1Y3Rpb25zX3Blcl9wcm9jZXNzKSk7XG4gICAgfSBlbHNlIHtcbiAgICAgIGxldCBxdWV1ZSA9IGVudHJ5LnZhbHVlWzFdO1xuICAgICAgbGV0IHRhc2sgPSBxdWV1ZS5uZXh0KCk7XG4gICAgICB0aGlzLmlzUnVubmluZyA9IHRydWU7XG5cbiAgICAgIGxldCByZXN1bHQ7XG5cbiAgICAgIHRyeSB7XG4gICAgICAgIHJlc3VsdCA9IHRhc2soKTtcbiAgICAgIH0gY2F0Y2ggKGUpIHtcbiAgICAgICAgY29uc29sZS5lcnJvcihlKTtcbiAgICAgICAgcmVzdWx0ID0gZTtcbiAgICAgIH1cblxuICAgICAgdGhpcy5pc1J1bm5pbmcgPSBmYWxzZTtcblxuICAgICAgaWYgKHJlc3VsdCBpbnN0YW5jZW9mIEVycm9yKSB7XG4gICAgICAgIHRocm93IHJlc3VsdDtcbiAgICAgIH1cblxuICAgICAgdGhpcy5pbnZva2VMYXRlcih0aGlzLmRvX3J1bihlbnRyeSwgcXVldWVJdGVyYXRvciwgcmVkdWN0aW9ucyAtIDEpKTtcbiAgICB9XG4gIH1cblxuICBhZGRUb1NjaGVkdWxlcihwaWQsIHRhc2ssIGR1ZVRpbWUgPSAwKSB7XG4gICAgaWYgKGR1ZVRpbWUgPT09IDApIHtcbiAgICAgIHRoaXMuaW52b2tlTGF0ZXIoKCkgPT4ge1xuICAgICAgICB0aGlzLmFkZFRvUXVldWUocGlkLCB0YXNrKTtcbiAgICAgIH0pO1xuICAgIH0gZWxzZSB7XG4gICAgICBzZXRUaW1lb3V0KCgpID0+IHtcbiAgICAgICAgdGhpcy5hZGRUb1F1ZXVlKHBpZCwgdGFzayk7XG4gICAgICB9LCBkdWVUaW1lKTtcbiAgICB9XG4gIH1cblxuICBzY2hlZHVsZShwaWQsIHRhc2spIHtcbiAgICB0aGlzLmFkZFRvU2NoZWR1bGVyKHBpZCwgKCkgPT4ge1xuICAgICAgdGFzaygpO1xuICAgIH0pO1xuICB9XG5cbiAgc2NoZWR1bGVGdXR1cmUocGlkLCBkdWVUaW1lLCB0YXNrKSB7XG4gICAgdGhpcy5hZGRUb1NjaGVkdWxlcihwaWQsICgpID0+IHtcbiAgICAgIHRhc2soKTtcbiAgICB9LCBkdWVUaW1lKTtcbiAgfVxufVxuXG5sZXQgcHJvY2Vzc19jb3VudGVyID0gLTE7XG5cbmNsYXNzIFBJRCB7XG4gIGNvbnN0cnVjdG9yKCkge1xuICAgIHByb2Nlc3NfY291bnRlciA9IHByb2Nlc3NfY291bnRlciArIDE7XG4gICAgdGhpcy5pZCA9IHByb2Nlc3NfY291bnRlcjtcbiAgfVxuXG4gIHRvU3RyaW5nKCkge1xuICAgIHJldHVybiBcIlBJRCM8MC5cIiArIHRoaXMuaWQgKyBcIi4wPlwiO1xuICB9XG59XG5cbmNsYXNzIFByb2Nlc3NTeXN0ZW0ge1xuXG4gIGNvbnN0cnVjdG9yKCkge1xuICAgIHRoaXMucGlkcyA9IG5ldyBNYXAoKTtcbiAgICB0aGlzLm1haWxib3hlcyA9IG5ldyBNYXAoKTtcbiAgICB0aGlzLm5hbWVzID0gbmV3IE1hcCgpO1xuICAgIHRoaXMubGlua3MgPSBuZXcgTWFwKCk7XG5cbiAgICBjb25zdCB0aHJvdHRsZSA9IDU7IC8vbXMgYmV0d2VlbiBzY2hlZHVsZWQgdGFza3NcbiAgICB0aGlzLmN1cnJlbnRfcHJvY2VzcyA9IG51bGw7XG4gICAgdGhpcy5zY2hlZHVsZXIgPSBuZXcgU2NoZWR1bGVyKHRocm90dGxlKTtcbiAgICB0aGlzLnN1c3BlbmRlZCA9IG5ldyBNYXAoKTtcblxuICAgIGxldCBwcm9jZXNzX3N5c3RlbV9zY29wZSA9IHRoaXM7XG4gICAgdGhpcy5tYWluX3Byb2Nlc3NfcGlkID0gdGhpcy5zcGF3bihmdW5jdGlvbiogKCkge1xuICAgICAgd2hpbGUgKHRydWUpIHtcbiAgICAgICAgeWllbGQgcHJvY2Vzc19zeXN0ZW1fc2NvcGUuc2xlZXAoMTAwMDApO1xuICAgICAgfVxuICAgIH0pO1xuICAgIHRoaXMuc2V0X2N1cnJlbnQodGhpcy5tYWluX3Byb2Nlc3NfcGlkKTtcbiAgfVxuXG4gIHN0YXRpYyAqcnVuKGZ1biwgYXJncywgY29udGV4dCA9IG51bGwpIHtcbiAgICBpZiAoZnVuLmNvbnN0cnVjdG9yLm5hbWUgPT09IFwiR2VuZXJhdG9yRnVuY3Rpb25cIikge1xuICAgICAgcmV0dXJuIHlpZWxkKiBmdW4uYXBwbHkoY29udGV4dCwgYXJncyk7XG4gICAgfSBlbHNlIHtcbiAgICAgIHJldHVybiBmdW4uYXBwbHkoY29udGV4dCwgYXJncyk7XG4gICAgfVxuICB9XG5cbiAgc3Bhd24oLi4uYXJncykge1xuICAgIGlmIChhcmdzLmxlbmd0aCA9PT0gMSkge1xuICAgICAgbGV0IGZ1biA9IGFyZ3NbMF07XG4gICAgICByZXR1cm4gdGhpcy5hZGRfcHJvYyhmdW4sIFtdLCBmYWxzZSkucGlkO1xuICAgIH0gZWxzZSBpZiAoYXJncy5sZW5ndGggPT09IDMpIHtcbiAgICAgIGxldCBtb2QgPSBhcmdzWzBdO1xuICAgICAgbGV0IGZ1biA9IGFyZ3NbMV07XG4gICAgICBsZXQgdGhlX2FyZ3MgPSBhcmdzWzJdO1xuXG4gICAgICByZXR1cm4gdGhpcy5hZGRfcHJvYyhtb2RbZnVuXSwgdGhlX2FyZ3MsIGZhbHNlKS5waWQ7XG4gICAgfVxuICB9XG5cbiAgc3Bhd25fbGluayguLi5hcmdzKSB7XG4gICAgaWYgKGFyZ3MubGVuZ3RoID09PSAxKSB7XG4gICAgICBsZXQgZnVuID0gYXJnc1swXTtcbiAgICAgIHJldHVybiB0aGlzLmFkZF9wcm9jKGZ1biwgW10sIHRydWUpLnBpZDtcbiAgICB9IGVsc2UgaWYgKGFyZ3MubGVuZ3RoID09PSAzKSB7XG4gICAgICBsZXQgbW9kID0gYXJnc1swXTtcbiAgICAgIGxldCBmdW4gPSBhcmdzWzFdO1xuICAgICAgbGV0IHRoZV9hcmdzID0gYXJnc1syXTtcblxuICAgICAgcmV0dXJuIHRoaXMuYWRkX3Byb2MobW9kW2Z1bl0sIHRoZV9hcmdzLCB0cnVlKS5waWQ7XG4gICAgfVxuICB9XG5cbiAgbGluayhwaWQpIHtcbiAgICB0aGlzLmxpbmtzLmdldCh0aGlzLnBpZCgpKS5hZGQocGlkKTtcbiAgICB0aGlzLmxpbmtzLmdldChwaWQpLmFkZCh0aGlzLnBpZCgpKTtcbiAgfVxuXG4gIHVubGluayhwaWQpIHtcbiAgICB0aGlzLmxpbmtzLmdldCh0aGlzLnBpZCgpKS5kZWxldGUocGlkKTtcbiAgICB0aGlzLmxpbmtzLmdldChwaWQpLmRlbGV0ZSh0aGlzLnBpZCgpKTtcbiAgfVxuXG4gIHNldF9jdXJyZW50KGlkKSB7XG4gICAgbGV0IHBpZCA9IHRoaXMucGlkb2YoaWQpO1xuICAgIGlmIChwaWQgIT09IG51bGwpIHtcbiAgICAgIHRoaXMuY3VycmVudF9wcm9jZXNzID0gdGhpcy5waWRzLmdldChwaWQpO1xuICAgICAgdGhpcy5jdXJyZW50X3Byb2Nlc3Muc3RhdHVzID0gU3RhdGVzLlJVTk5JTkc7XG4gICAgfVxuICB9XG5cbiAgYWRkX3Byb2MoZnVuLCBhcmdzLCBsaW5rZWQpIHtcbiAgICBsZXQgbmV3cGlkID0gbmV3IFBJRCgpO1xuICAgIGxldCBtYWlsYm94ID0gbmV3IE1haWxib3goKTtcbiAgICBsZXQgbmV3cHJvYyA9IG5ldyBQcm9jZXNzKG5ld3BpZCwgZnVuLCBhcmdzLCBtYWlsYm94LCB0aGlzKTtcblxuICAgIHRoaXMucGlkcy5zZXQobmV3cGlkLCBuZXdwcm9jKTtcbiAgICB0aGlzLm1haWxib3hlcy5zZXQobmV3cGlkLCBtYWlsYm94KTtcbiAgICB0aGlzLmxpbmtzLnNldChuZXdwaWQsIG5ldyBTZXQoKSk7XG5cbiAgICBpZiAobGlua2VkKSB7XG4gICAgICB0aGlzLmxpbmsobmV3cGlkKTtcbiAgICB9XG5cbiAgICBuZXdwcm9jLnN0YXJ0KCk7XG4gICAgcmV0dXJuIG5ld3Byb2M7XG4gIH1cblxuICByZW1vdmVfcHJvYyhwaWQsIGV4aXRyZWFzb24pIHtcbiAgICB0aGlzLnBpZHMuZGVsZXRlKHBpZCk7XG4gICAgdGhpcy51bnJlZ2lzdGVyKHBpZCk7XG4gICAgdGhpcy5zY2hlZHVsZXIucmVtb3ZlUGlkKHBpZCk7XG5cbiAgICBpZiAodGhpcy5saW5rcy5oYXMocGlkKSkge1xuICAgICAgZm9yIChsZXQgbGlua3BpZCBvZiB0aGlzLmxpbmtzLmdldChwaWQpKSB7XG4gICAgICAgIHRoaXMuZXhpdChsaW5rcGlkLCBleGl0cmVhc29uKTtcbiAgICAgICAgdGhpcy5saW5rcy5nZXQobGlua3BpZCkuZGVsZXRlKHBpZCk7XG4gICAgICB9XG5cbiAgICAgIHRoaXMubGlua3MuZGVsZXRlKHBpZCk7XG4gICAgfVxuICB9XG5cbiAgcmVnaXN0ZXIobmFtZSwgcGlkKSB7XG4gICAgaWYgKCF0aGlzLm5hbWVzLmhhcyhuYW1lKSkge1xuICAgICAgdGhpcy5uYW1lcy5zZXQobmFtZSwgcGlkKTtcbiAgICB9IGVsc2Uge1xuICAgICAgdGhyb3cgbmV3IEVycm9yKFwiTmFtZSBpcyBhbHJlYWR5IHJlZ2lzdGVyZWQgdG8gYW5vdGhlciBwcm9jZXNzXCIpO1xuICAgIH1cbiAgfVxuXG4gIHJlZ2lzdGVyZWQobmFtZSkge1xuICAgIHJldHVybiB0aGlzLm5hbWVzLmhhcyhuYW1lKSA/IHRoaXMubmFtZXMuZ2V0KG5hbWUpIDogbnVsbDtcbiAgfVxuXG4gIHVucmVnaXN0ZXIocGlkKSB7XG4gICAgZm9yIChsZXQgbmFtZSBvZiB0aGlzLm5hbWVzLmtleXMoKSkge1xuICAgICAgaWYgKHRoaXMubmFtZXMuaGFzKG5hbWUpICYmIHRoaXMubmFtZXMuZ2V0KG5hbWUpID09PSBwaWQpIHtcbiAgICAgICAgdGhpcy5uYW1lcy5kZWxldGUobmFtZSk7XG4gICAgICB9XG4gICAgfVxuICB9XG5cbiAgcGlkKCkge1xuICAgIHJldHVybiB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5waWQ7XG4gIH1cblxuICBwaWRvZihpZCkge1xuICAgIGlmIChpZCBpbnN0YW5jZW9mIFBJRCkge1xuICAgICAgcmV0dXJuIHRoaXMucGlkcy5oYXMoaWQpID8gaWQgOiBudWxsO1xuICAgIH0gZWxzZSBpZiAoaWQgaW5zdGFuY2VvZiBQcm9jZXNzKSB7XG4gICAgICByZXR1cm4gaWQucGlkO1xuICAgIH0gZWxzZSB7XG4gICAgICBsZXQgcGlkID0gdGhpcy5yZWdpc3RlcmVkKGlkKTtcbiAgICAgIGlmIChwaWQgPT09IG51bGwpIHRocm93IFwiUHJvY2VzcyBuYW1lIG5vdCByZWdpc3RlcmVkOiBcIiArIGlkICsgXCIgKFwiICsgdHlwZW9mIGlkICsgXCIpXCI7XG4gICAgICByZXR1cm4gcGlkO1xuICAgIH1cbiAgfVxuXG4gIHNlbmQoaWQsIG1zZykge1xuICAgIGNvbnN0IHBpZCA9IHRoaXMucGlkb2YoaWQpO1xuXG4gICAgaWYgKHBpZCkge1xuICAgICAgdGhpcy5tYWlsYm94ZXMuZ2V0KHBpZCkuZGVsaXZlcihtc2cpO1xuXG4gICAgICBpZiAodGhpcy5zdXNwZW5kZWQuaGFzKHBpZCkpIHtcbiAgICAgICAgbGV0IGZ1biA9IHRoaXMuc3VzcGVuZGVkLmdldChwaWQpO1xuICAgICAgICB0aGlzLnN1c3BlbmRlZC5kZWxldGUocGlkKTtcbiAgICAgICAgdGhpcy5zY2hlZHVsZShmdW4pO1xuICAgICAgfVxuICAgIH1cblxuICAgIHJldHVybiBtc2c7XG4gIH1cblxuICByZWNlaXZlKGZ1biwgdGltZW91dCA9IDAsIHRpbWVvdXRGbiA9ICgpID0+IHRydWUpIHtcbiAgICBsZXQgRGF0ZVRpbWVvdXQgPSBudWxsO1xuXG4gICAgaWYgKHRpbWVvdXQgPT09IDAgfHwgdGltZW91dCA9PT0gSW5maW5pdHkpIHtcbiAgICAgIERhdGVUaW1lb3V0ID0gbnVsbDtcbiAgICB9IGVsc2Uge1xuICAgICAgRGF0ZVRpbWVvdXQgPSBEYXRlLm5vdygpICsgdGltZW91dDtcbiAgICB9XG5cbiAgICByZXR1cm4gW1N0YXRlcy5SRUNFSVZFLCBmdW4sIERhdGVUaW1lb3V0LCB0aW1lb3V0Rm5dO1xuICB9XG5cbiAgc2xlZXAoZHVyYXRpb24pIHtcbiAgICByZXR1cm4gW1N0YXRlcy5TTEVFUCwgZHVyYXRpb25dO1xuICB9XG5cbiAgc3VzcGVuZChmdW4pIHtcbiAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5zdGF0dXMgPSBTdGF0ZXMuU1VTUEVOREVEO1xuICAgIHRoaXMuc3VzcGVuZGVkLnNldCh0aGlzLmN1cnJlbnRfcHJvY2Vzcy5waWQsIGZ1bik7XG4gIH1cblxuICBkZWxheShmdW4sIHRpbWUpIHtcbiAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5zdGF0dXMgPSBTdGF0ZXMuU0xFRVBJTkc7XG4gICAgdGhpcy5zY2hlZHVsZXIuc2NoZWR1bGVGdXR1cmUodGhpcy5jdXJyZW50X3Byb2Nlc3MucGlkLCB0aW1lLCBmdW4pO1xuICB9XG5cbiAgc2NoZWR1bGUoZnVuLCBwaWQpIHtcbiAgICBjb25zdCB0aGVfcGlkID0gcGlkICE9IG51bGwgPyBwaWQgOiB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5waWQ7XG4gICAgdGhpcy5zY2hlZHVsZXIuc2NoZWR1bGUodGhlX3BpZCwgZnVuKTtcbiAgfVxuXG4gIGV4aXQob25lLCB0d28pIHtcbiAgICBpZiAodHdvKSB7XG4gICAgICBsZXQgcGlkID0gb25lO1xuICAgICAgbGV0IHJlYXNvbiA9IHR3bztcblxuICAgICAgbGV0IHByb2Nlc3MgPSB0aGlzLnBpZHMuZ2V0KHRoaXMucGlkb2YocGlkKSk7XG4gICAgICBpZiAocHJvY2VzcyAmJiBwcm9jZXNzLmlzX3RyYXBwaW5nX2V4aXRzKCkgfHwgcmVhc29uID09PSBTdGF0ZXMuS0lMTCB8fCByZWFzb24gPT09IFN0YXRlcy5OT1JNQUwpIHtcbiAgICAgICAgdGhpcy5tYWlsYm94ZXMuZ2V0KHByb2Nlc3MucGlkKS5kZWxpdmVyKFtTdGF0ZXMuRVhJVCwgdGhpcy5waWQoKSwgcmVhc29uXSk7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICBwcm9jZXNzLnNpZ25hbChyZWFzb24pO1xuICAgICAgfVxuICAgIH0gZWxzZSB7XG4gICAgICBsZXQgcmVhc29uID0gb25lO1xuICAgICAgdGhpcy5jdXJyZW50X3Byb2Nlc3Muc2lnbmFsKHJlYXNvbik7XG4gICAgfVxuICB9XG5cbiAgZXJyb3IocmVhc29uKSB7XG4gICAgdGhpcy5jdXJyZW50X3Byb2Nlc3Muc2lnbmFsKHJlYXNvbik7XG4gIH1cblxuICBwcm9jZXNzX2ZsYWcoZmxhZywgdmFsdWUpIHtcbiAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5wcm9jZXNzX2ZsYWcoZmxhZywgdmFsdWUpO1xuICB9XG5cbiAgcHV0KGtleSwgdmFsdWUpIHtcbiAgICB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0W2tleV0gPSB2YWx1ZTtcbiAgfVxuXG4gIGdldChrZXkpIHtcbiAgICBpZiAoa2V5ICE9IG51bGwpIHtcbiAgICAgIHJldHVybiB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0W2tleV07XG4gICAgfSBlbHNlIHtcbiAgICAgIHJldHVybiB0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0O1xuICAgIH1cbiAgfVxuXG4gIGdldF9rZXlzKCkge1xuICAgIHJldHVybiBPYmplY3Qua2V5cyh0aGlzLmN1cnJlbnRfcHJvY2Vzcy5kaWN0KTtcbiAgfVxuXG4gIGVyYXNlKGtleSkge1xuICAgIGlmIChrZXkgIT0gbnVsbCkge1xuICAgICAgZGVsZXRlIHRoaXMuY3VycmVudF9wcm9jZXNzLmRpY3Rba2V5XTtcbiAgICB9IGVsc2Uge1xuICAgICAgdGhpcy5jdXJyZW50X3Byb2Nlc3MuZGljdCA9IHt9O1xuICAgIH1cbiAgfVxufVxuXG5mdW5jdGlvbiBzdGFydChtb2R1bGUsIGFyZ3MpIHtcbiAgcmV0dXJuIFtTeW1ib2wuZm9yKFwib2tcIiksIHNlbGYuc3lzdGVtLnNwYXduKHN0YXJ0X3Byb2Nlc3MobW9kdWxlLCBhcmdzKSldO1xufVxuXG5mdW5jdGlvbiBzdGFydF9saW5rKG1vZHVsZSwgYXJncykge1xuICByZXR1cm4gW1N5bWJvbC5mb3IoXCJva1wiKSwgc2VsZi5zeXN0ZW0uc3Bhd25fbGluayhzdGFydF9wcm9jZXNzKG1vZHVsZSwgYXJncykpXTtcbn1cblxuZnVuY3Rpb24gc3RhcnRfcHJvY2Vzcyhtb2R1bGUsIGFyZ3MpIHtcbiAgcmV0dXJuIGZ1bmN0aW9uKiAoKSB7XG4gICAgbGV0IFtvaywgc3RhdGVdID0gbW9kdWxlLmluaXQuYXBwbHkobnVsbCwgW2FyZ3NdKTtcbiAgICB5aWVsZCBzZWxmLnN5c3RlbS5wdXQoXCJzdGF0ZVwiLCBzdGF0ZSk7XG5cbiAgICB0cnkge1xuICAgICAgd2hpbGUgKHRydWUpIHtcbiAgICAgICAgeWllbGQgc2VsZi5zeXN0ZW0ucmVjZWl2ZShmdW5jdGlvbiAoYXJncykge1xuICAgICAgICAgIGxldCBjb21tYW5kID0gYXJnc1swXTtcblxuICAgICAgICAgIHN3aXRjaCAoY29tbWFuZCkge1xuICAgICAgICAgICAgY2FzZSBcImNhbGxcIjpcbiAgICAgICAgICAgICAgdmFyIHJlcXVlc3QgPSBhcmdzWzFdO1xuICAgICAgICAgICAgICB2YXIgc2VuZGVyID0gYXJnc1syXTtcblxuICAgICAgICAgICAgICB2YXIgW3JlcGx5LCByZXNwb25zZSwgbmV3X3N0YXRlXSA9IG1vZHVsZS5oYW5kbGVfY2FsbChyZXF1ZXN0LCBzZW5kZXIsIHNlbGYuc3lzdGVtLmdldChcInN0YXRlXCIpKTtcbiAgICAgICAgICAgICAgc2VsZi5zeXN0ZW0ucHV0KFwic3RhdGVcIiwgbmV3X3N0YXRlKTtcblxuICAgICAgICAgICAgICBzZWxmLnN5c3RlbS5zZW5kKHNlbmRlciwgcmVzcG9uc2UpO1xuICAgICAgICAgICAgICBicmVhaztcblxuICAgICAgICAgICAgY2FzZSBcImNhc3RcIjpcbiAgICAgICAgICAgICAgdmFyIHJlcXVlc3QgPSBhcmdzWzFdO1xuICAgICAgICAgICAgICB2YXIgc2VuZGVyID0gYXJnc1syXTtcblxuICAgICAgICAgICAgICB2YXIgW3JlcGx5LCBuZXdfc3RhdGVdID0gbW9kdWxlLmhhbmRsZV9jYXN0KHJlcXVlc3QsIHNlbGYuc3lzdGVtLmdldChcInN0YXRlXCIpKTtcblxuICAgICAgICAgICAgICBzZWxmLnN5c3RlbS5wdXQoXCJzdGF0ZVwiLCBuZXdfc3RhdGUpO1xuICAgICAgICAgICAgICBzZWxmLnN5c3RlbS5zZW5kKGFyZ3NbMl0sIFN5bWJvbC5mb3IoXCJva1wiKSk7XG5cbiAgICAgICAgICAgICAgYnJlYWs7XG5cbiAgICAgICAgICAgIGNhc2UgXCJzdG9wXCI6XG4gICAgICAgICAgICAgIHRocm93IFwic3RvcFwiO1xuICAgICAgICAgIH1cbiAgICAgICAgfSk7XG4gICAgICB9XG4gICAgfSBjYXRjaCAoZSkge1xuICAgICAgaWYgKGUgIT09IFwic3RvcFwiKSB7XG4gICAgICAgIHRocm93IGU7XG4gICAgICB9XG4gICAgfVxuICB9O1xufVxuXG5mdW5jdGlvbiogY2FsbChzZXJ2ZXIsIHJlcXVlc3QpIHtcbiAgc2VsZi5zeXN0ZW0uc2VuZChzZXJ2ZXIsIFtcImNhbGxcIiwgcmVxdWVzdCwgc2VsZi5zeXN0ZW0ucGlkKCldKTtcblxuICByZXR1cm4geWllbGQgc2VsZi5zeXN0ZW0ucmVjZWl2ZShmdW5jdGlvbiAoYXJncykge1xuICAgIHJldHVybiBhcmdzO1xuICB9KTtcbn1cblxuZnVuY3Rpb24qIGNhc3Qoc2VydmVyLCByZXF1ZXN0KSB7XG4gIHNlbGYuc3lzdGVtLnNlbmQoc2VydmVyLCBbXCJjYXN0XCIsIHJlcXVlc3QsIHNlbGYuc3lzdGVtLnBpZCgpXSk7XG5cbiAgcmV0dXJuIHlpZWxkIHNlbGYuc3lzdGVtLnJlY2VpdmUoZnVuY3Rpb24gKGFyZ3MpIHtcbiAgICByZXR1cm4gYXJncztcbiAgfSk7XG59XG5cbmZ1bmN0aW9uIHN0b3Aoc2VydmVyKSB7XG4gIHNlbGYuc3lzdGVtLnNlbmQoc2VydmVyLCBbXCJzdG9wXCJdKTtcbn1cblxudmFyIGdlbl9zZXJ2ZXIgPSB7IHN0YXJ0LCBzdGFydF9saW5rLCBjYWxsLCBjYXN0LCBzdG9wIH07XG5cbmV4cG9ydCB7IFByb2Nlc3NTeXN0ZW0sIGdlbl9zZXJ2ZXIgYXMgR2VuU2VydmVyIH07Il0sInNvdXJjZVJvb3QiOiIvc291cmNlLyJ9
