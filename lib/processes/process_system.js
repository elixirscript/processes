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
    }
    demonitor(ref) {
        if (this.monitors.has(ref)) {
            this.monitors.delete(ref);
            return true;
        }
        return false;
    }
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
    register(name, pid) {
        if (!this.names.has(name)) {
            this.names.set(name, pid);
        }
        else {
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
        if (this.currentProcess) {
            return this.currentProcess.pid;
        }
        return null;
    }
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
    sleep(duration) {
        return [states_1.default.SLEEP, duration];
    }
    suspend(fun) {
        if (this.currentProcess) {
            this.currentProcess.status = states_1.default.SUSPENDED;
            this.suspended.set(this.currentProcess.pid, fun);
        }
    }
    delay(fun, time) {
        if (this.currentProcess) {
            this.currentProcess.status = states_1.default.SLEEPING;
            if (Number.isInteger(time)) {
                this.scheduler.scheduleFuture(this.currentProcess.pid, time, fun);
            }
        }
    }
    schedule(fun, pid) {
        if (this.currentProcess) {
            const the_pid = pid != null ? pid : this.currentProcess.pid;
            this.scheduler.schedule(the_pid, fun);
        }
    }
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
    error(reason) {
        if (this.currentProcess) {
            this.currentProcess.signal(reason);
        }
    }
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
    put(key, value) {
        if (this.currentProcess) {
            this.currentProcess.dict.set(key, value);
        }
    }
    get_process_dict() {
        if (this.currentProcess) {
            return this.currentProcess.dict;
        }
        throw new Error('No Current Process');
    }
    get(key, default_value = null) {
        if (this.currentProcess && key in this.currentProcess.dict) {
            return this.currentProcess.dict.get(key);
        }
        else {
            return default_value;
        }
    }
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
    is_alive(pid) {
        const real_pid = this.pidof(pid);
        return real_pid != null;
    }
    list() {
        return Array.from(this.pids.keys());
    }
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
