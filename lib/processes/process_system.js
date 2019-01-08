/* @flow */
'use strict';
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const process_1 = __importDefault(require("./process"));
const states_1 = __importDefault(require("./states"));
const default_scheduler_1 = __importDefault(require("./schedulers/default_scheduler"));
const erlang_types_1 = require("erlang-types");
/**
 * Manages all of the processes.
 */
class ProcessSystem {
    constructor(scheduler = new default_scheduler_1.default(5)) {
        this.pids = new Map();
        this.mailboxes = new Map();
        this.names = new Map();
        this.links = new Map();
        this.monitors = new Map();
        this.current_process = null;
        this.scheduler = scheduler;
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
        }
        else {
            return yield fun.apply(context, args);
        }
    }
    /**
     * Starts a process represented by the given generator function
     * @param args Either a generator function or a module, function and arguments
     */
    spawn(...args) {
        if (args.length === 1) {
            let fun = args[0];
            return this.add_proc(fun, [], false, false).pid;
        }
        else {
            let mod = args[0];
            let fun = args[1];
            let the_args = args[2];
            return this.add_proc(mod[fun], the_args, false, false).pid;
        }
    }
    /**
     * Starts a process using the generator function from the specified module
     * @param args Either a generator function or a module, function and arguments
     */
    spawn_link(...args) {
        if (args.length === 1) {
            let fun = args[0];
            return this.add_proc(fun, [], true, false).pid;
        }
        else {
            let mod = args[0];
            let fun = args[1];
            let the_args = args[2];
            return this.add_proc(mod[fun], the_args, true, false).pid;
        }
    }
    /**
     * links the current process with the process from the given pid
     * @param pid pid of the process to link to
     */
    link(pid) {
        const currentProcessPid = this.pid();
        if (currentProcessPid != null) {
            const currentProcessLink = this.links.get(currentProcessPid);
            const IncomingProcessLink = this.links.get(pid);
            if (currentProcessLink && IncomingProcessLink) {
                currentProcessLink.add(pid);
                IncomingProcessLink.add(currentProcessPid);
            }
        }
    }
    /**
     * unlinks the current process from the process from the given pid
     * @param pid pid of the process to link to
     */
    unlink(pid) {
        const currentProcessPid = this.pid();
        if (currentProcessPid != null) {
            const currentProcessLink = this.links.get(currentProcessPid);
            const IncomingProcessLink = this.links.get(pid);
            if (currentProcessLink && IncomingProcessLink) {
                currentProcessLink.delete(pid);
                IncomingProcessLink.delete(currentProcessPid);
            }
        }
    }
    /**
     * Spawns a process and then monitors it
     * @param args Either a generator function or a module, function and arguments
     */
    spawn_monitor(...args) {
        if (args.length === 1) {
            let fun = args[0];
            let process = this.add_proc(fun, [], false, true);
            return [process.pid, process.monitors[0]];
        }
        else {
            let mod = args[0];
            let fun = args[1];
            let the_args = args[2];
            let process = this.add_proc(mod[fun], the_args, false, true);
            return [process.pid, process.monitors[0]];
        }
    }
    /**
     * Monitors the given process
     * @param pid pid of the process to link to
     */
    monitor(pid) {
        const real_pid = this.pidof(pid);
        const ref = this.make_ref();
        if (this.currentProcess != null) {
            if (real_pid) {
                this.monitors.set(ref, {
                    monitor: this.currentProcess.pid,
                    monitee: real_pid,
                });
                const process = this.pids.get(real_pid);
                if (process) {
                    process.monitors.push(ref);
                }
                return ref;
            }
            else {
                this.send(this.currentProcess.pid, new erlang_types_1.Tuple('DOWN', ref, pid, real_pid, Symbol.for('noproc')));
                return ref;
            }
        }
        return null;
    }
    /**
     * Removes the monitor
     * @param ref Reference to monitor
     */
    demonitor(ref) {
        if (this.monitors.has(ref)) {
            this.monitors.delete(ref);
            return true;
        }
        return false;
    }
    /**
     * Sets the current process
     * @param id PID or name of process
     */
    set_current(id) {
        let pid = this.pidof(id);
        if (pid) {
            const next = this.pids.get(pid);
            if (next) {
                this.current_process = next;
                if (this.currentProcess) {
                    this.currentProcess.status = states_1.default.RUNNING;
                }
            }
        }
    }
    add_proc(fun, args, linked, monitored) {
        let newproc = new process_1.default(this, fun, args);
        this.pids.set(newproc.pid, newproc);
        this.mailboxes.set(newproc.pid, newproc.mailbox);
        this.links.set(newproc.pid, new Set());
        if (linked) {
            this.link(newproc.pid);
        }
        if (monitored) {
            this.monitor(newproc.pid);
        }
        newproc.start();
        return newproc;
    }
    remove_proc(pid, exitreason) {
        this.pids.delete(pid);
        this.unregister(pid);
        this.scheduler.removePid(pid);
        const linkedPids = this.links.get(pid);
        if (linkedPids) {
            for (let linkpid of linkedPids) {
                this.exit(linkpid, exitreason);
                const linkedPid = this.links.get(linkpid);
                if (linkedPid) {
                    linkedPid.delete(pid);
                }
            }
            this.links.delete(pid);
        }
    }
    /**
     * registers the given name to the pid
     * @param name The name to give the process
     * @param pid The pid of the process
     */
    register(name, pid) {
        if (!this.names.has(name)) {
            this.names.set(name, pid);
        }
        else {
            throw new Error('Name is already registered to another process');
        }
    }
    /**
     * Finds a process by the given name
     * @param name the name of the process
     */
    whereis(name) {
        return this.names.has(name) ? this.names.get(name) : null;
    }
    /**
     * returns the liast of names that are registered
     */
    registered() {
        return this.names.keys();
    }
    /**
     * unregisters the names associated with the pid
     * @param pid The pid of the process
     */
    unregister(pid) {
        for (let name of this.names.keys()) {
            if (this.names.has(name) && this.names.get(name) === pid) {
                this.names.delete(name);
            }
        }
    }
    /**
     * Returns the PID of the current process
     */
    pid() {
        if (this.currentProcess) {
            return this.currentProcess.pid;
        }
        return null;
    }
    /**
     * takes the input and tries to find the pid. Input can be a `pid`, `Process`, or name the pid is associated with
     * @param id The registered name or pid of the process
     */
    pidof(id) {
        if (id instanceof erlang_types_1.PID) {
            return this.pids.has(id) ? id : null;
        }
        else if (id instanceof process_1.default) {
            return id.pid;
        }
        else {
            let pid = this.whereis(id);
            if (pid === null)
                throw 'Process name not registered: ' + id + ' (' + typeof id + ')';
            return pid;
        }
    }
    /**
     * sends a message the the process represented by the pid
     * @param id
     * @param msg
     */
    send(id, msg) {
        const pid = this.pidof(id);
        if (pid) {
            const mailbox = this.mailboxes.get(pid);
            if (mailbox) {
                mailbox.deliver(msg);
            }
            if (this.suspended.has(pid)) {
                let fun = this.suspended.get(pid);
                this.suspended.delete(pid);
                if (fun) {
                    this.schedule(fun);
                }
            }
        }
        return msg;
    }
    /**
     * Tells the current process to receive a message that the function can handle.
     * If no match then the process is put in the suspended state until a message arrives
     * or the timeout is reached.
     * If the timeout is reached and no msg matches, then the timeoutFn is called
     * @param fun
     * @param timeout
     * @param timeoutFn
     */
    receive(fun, timeout = 0, timeoutFn = () => true) {
        let DateTimeout = null;
        if (timeout === 0 || timeout === Infinity) {
            DateTimeout = null;
        }
        else {
            DateTimeout = Date.now() + timeout;
        }
        return [states_1.default.RECEIVE, fun, DateTimeout, timeoutFn];
    }
    /**
     * puts the current process to sleep
     * @param duration
     */
    sleep(duration) {
        return [states_1.default.SLEEP, duration];
    }
    /**
     * Suspends the current process
     * @param fun
     */
    suspend(fun) {
        if (this.currentProcess) {
            this.currentProcess.status = states_1.default.SUSPENDED;
            this.suspended.set(this.currentProcess.pid, fun);
        }
    }
    /**
     * Makes current process go to sleep
     * @param fun
     * @param time
     */
    delay(fun, time) {
        if (this.currentProcess) {
            this.currentProcess.status = states_1.default.SLEEPING;
            if (Number.isInteger(time)) {
                this.scheduler.scheduleFuture(this.currentProcess.pid, time, fun);
            }
        }
    }
    /**
     * Schedules execution of a process reduction
     * @param fun
     * @param pid
     */
    schedule(fun, pid) {
        if (this.currentProcess) {
            const the_pid = pid != null ? pid : this.currentProcess.pid;
            this.scheduler.schedule(the_pid, fun);
        }
    }
    /**
     * terminates the current process with the given reason.
     * @param one
     * @param two
     */
    exit(one, two) {
        let pid = null;
        let reason = null;
        let process = null;
        if (two) {
            pid = one;
            reason = two;
            const thePid = this.pidof(pid);
            if (thePid) {
                process = this.pids.get(thePid);
            }
            if (process) {
                if (process.is_trapping_exits() ||
                    reason === states_1.default.KILL ||
                    reason === states_1.default.NORMAL) {
                    const mailbox = this.mailboxes.get(process.pid);
                    if (mailbox) {
                        mailbox.deliver(new erlang_types_1.Tuple(states_1.default.EXIT, this.pid(), reason));
                    }
                }
                else {
                    process.signal(reason);
                }
            }
        }
        else {
            if (this.currentProcess) {
                pid = this.currentProcess.pid;
                reason = one;
                process = this.currentProcess;
                process.signal(reason);
            }
        }
        if (process) {
            for (let ref of process.monitors) {
                let mons = this.monitors.get(ref);
                if (mons) {
                    this.send(mons['monitor'], new erlang_types_1.Tuple('DOWN', ref, mons['monitee'], mons['monitee'], reason));
                }
            }
        }
    }
    /**
     * terminates the current process with an error
     * @param reason
     */
    error(reason) {
        if (this.currentProcess) {
            this.currentProcess.signal(reason);
        }
    }
    /**
     * Sets flags on the current process.
      - Note: the only flag respected is the `Symbol.for("trap_exit")` flag.
      If value is `true`, then exit signals from linked processes are turned into
      messages and sent to the current processes mailbox.
      If value is `false`, the exit is treated as normal and terminates the process.
      Setting it to `true` is useful for supervising processes.
     * @param args
     */
    process_flag(...args) {
        if (args.length == 2) {
            const flag = args[0];
            const value = args[1];
            if (this.currentProcess) {
                return this.currentProcess.process_flag(flag, value);
            }
        }
        else {
            const pid = this.pidof(args[0]);
            if (pid) {
                const flag = args[1];
                const value = args[2];
                const process = this.pids.get(pid);
                if (process) {
                    return process.process_flag(flag, value);
                }
            }
        }
    }
    /**
     * Adds a value to the current process's dictionary
     * @param key
     * @param value
     */
    put(key, value) {
        if (this.currentProcess) {
            this.currentProcess.dict.set(key, value);
        }
    }
    /**
     * Gets the current process's dictionary
     */
    get_process_dict() {
        if (this.currentProcess) {
            return this.currentProcess.dict;
        }
        throw new Error('No Current Process');
    }
    /**
     * Gets a value from the current process's dictionary or the default if key not in dictionary
     * @param key
     * @param default_value
     */
    get(key, default_value = null) {
        if (this.currentProcess && key in this.currentProcess.dict) {
            return this.currentProcess.dict.get(key);
        }
        else {
            return default_value;
        }
    }
    /**
     * Gets all the keys from the current process's dictionary
     * @param value
     */
    get_keys(value) {
        if (value) {
            let keys = [];
            if (this.currentProcess) {
                for (let key of Object.keys(this.currentProcess.dict)) {
                    if (this.currentProcess.dict.get(key) === value) {
                        keys.push(key);
                    }
                }
            }
            return keys;
        }
        if (this.currentProcess) {
            return Object.keys(this.currentProcess.dict);
        }
        throw new Error('No Current Process');
    }
    /**
     * Removes the key and the associated value from the current process's dictionary
     *
     * If no key is given, removes all entries from the current process's dictionary
     * @param key the key to remove
     */
    erase(key) {
        if (this.currentProcess) {
            if (key != null && this.currentProcess.dict.has(key)) {
                this.currentProcess.dict.delete(key);
            }
            else {
                this.currentProcess.dict = new Map();
            }
        }
    }
    /**
     * Returns if the given pid is alive
     * @param pid
     */
    is_alive(pid) {
        const real_pid = this.pidof(pid);
        return real_pid != null;
    }
    /**
     * Returns a list of all the pids
     */
    list() {
        return Array.from(this.pids.keys());
    }
    /**
     * Returns a unique reference
     */
    make_ref() {
        return new erlang_types_1.Reference();
    }
    get currentProcess() {
        if (this.current_process) {
            return this.current_process;
        }
        return null;
    }
}
exports.default = ProcessSystem;
