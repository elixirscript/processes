'use strict';

var erlangTypes = require('erlang-types');

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
  NORMAL: Symbol.for('normal'),
  KILL: Symbol.for('kill'),
  SUSPEND: Symbol.for('suspend'),
  CONTINUE: Symbol.for('continue'),
  RECEIVE: Symbol.for('receive'),
  SEND: Symbol.for('send'),
  SLEEPING: Symbol.for('sleeping'),
  RUNNING: Symbol.for('running'),
  SUSPENDED: Symbol.for('suspended'),
  STOPPED: Symbol.for('stopped'),
  SLEEP: Symbol.for('sleep'),
  EXIT: Symbol.for('exit'),
  NOMATCH: Symbol.for('no_match')
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
    return this.flags[Symbol.for('trap_exit')] && this.flags[Symbol.for('trap_exit')] == true;
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
        if (e.constructor.name != 'MatchError') {
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
    }; // In our case a reduction is equal to a task call
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
      yield process_system_scope.sleep(Symbol.for('Infinity'));
    });
    this.set_current(this.main_process_pid);
  }

  static *run(fun, args, context = null) {
    if (fun.constructor.name === 'GeneratorFunction') {
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
      this.monitors.set(ref, {
        monitor: this.current_process.pid,
        monitee: real_pid
      });
      this.pids.get(real_pid).monitors.push(ref);
      return ref;
    } else {
      this.send(this.current_process.pid, new erlangTypes.Tuple('DOWN', ref, pid, real_pid, Symbol.for('noproc')));
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
    let newpid = new erlangTypes.PID();
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
      throw new Error('Name is already registered to another process');
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
    if (id instanceof erlangTypes.PID) {
      return this.pids.has(id) ? id : null;
    } else if (id instanceof Process) {
      return id.pid;
    } else {
      let pid = this.whereis(id);
      if (pid === null) throw 'Process name not registered: ' + id + ' (' + typeof id + ')';
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
        this.mailboxes.get(process.pid).deliver(new erlangTypes.Tuple(States.EXIT, this.pid(), reason));
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
      this.send(mons['monitor'], new erlangTypes.Tuple('DOWN', ref, mons['monitee'], mons['monitee'], reason));
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
    return new erlangTypes.Reference();
  }

}

var index = {
  ProcessSystem
};

module.exports = index;
