"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const states_1 = __importDefault(require("./states"));
function is_sleep(value) {
    return Array.isArray(value) && value[0] === states_1.default.SLEEP;
}
function is_receive(value) {
    return Array.isArray(value) && value[0] === states_1.default.RECEIVE;
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
        this.status = states_1.default.STOPPED;
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
        let retval = states_1.default.NORMAL;
        try {
            yield* this.func.apply(null, this.args);
        }
        catch (e) {
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
        if (reason !== states_1.default.NORMAL) {
            console.error(reason);
        }
        this.system.remove_proc(this.pid, reason);
    }
    receive(fun) {
        let value = states_1.default.NOMATCH;
        let messages = this.mailbox.get();
        for (let i = 0; i < messages.length; i++) {
            try {
                value = fun(messages[i]);
                if (value !== states_1.default.NOMATCH) {
                    this.mailbox.removeAt(i);
                    break;
                }
            }
            catch (e) {
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
            }
            else if (is_receive(value) && receive_timed_out(value)) {
                let result = value[3]();
                this.system.schedule(function () {
                    function_scope.system.set_current(function_scope.pid);
                    function_scope.run(machine, machine.next(result));
                });
            }
            else if (is_receive(value)) {
                let result = function_scope.receive(value[1]);
                if (result === states_1.default.NOMATCH) {
                    this.system.suspend(function () {
                        function_scope.system.set_current(function_scope.pid);
                        function_scope.run(machine, step);
                    });
                }
                else {
                    this.system.schedule(function () {
                        function_scope.system.set_current(function_scope.pid);
                        function_scope.run(machine, machine.next(result));
                    });
                }
            }
            else {
                this.system.schedule(function () {
                    function_scope.system.set_current(function_scope.pid);
                    function_scope.run(machine, machine.next(value));
                });
            }
        }
    }
}
exports.default = Process;
