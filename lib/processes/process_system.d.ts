import Mailbox from './mailbox';
import Process from './process';
import DefaultScheduler from './schedulers/default_scheduler';
import { PID, Reference } from 'erlang-types';
declare class ProcessSystem {
    pids: Map<PID, Process>;
    mailboxes: Map<PID, Mailbox>;
    names: Map<any, PID>;
    links: Map<PID, Set<PID>>;
    monitors: Map<Reference, {
        monitor: PID;
        monitee: PID;
    }>;
    current_process: Process | null;
    scheduler: DefaultScheduler;
    suspended: Map<PID, () => any>;
    main_process_pid: PID;
    constructor(scheduler?: DefaultScheduler);
    static run(fun: Function, args: any[], context?: any): IterableIterator<any>;
    spawn(...args: any[]): PID;
    spawn_link(...args: any[]): PID;
    link(pid: PID): void;
    unlink(pid: PID): void;
    spawn_monitor(...args: any[]): any[];
    monitor(pid: PID): Reference | undefined;
    demonitor(ref: Reference): boolean;
    set_current(id: any): void;
    add_proc(fun: GeneratorFunction, args: any[], linked: boolean, monitored: boolean): Process;
    remove_proc(pid: PID, exitreason: any): void;
    register(name: any, pid: PID): void;
    whereis(name: any): PID | null | undefined;
    registered(): IterableIterator<any>;
    unregister(pid: PID): void;
    pid(): PID | null;
    pidof(id: any): PID | null | undefined;
    send(id: any, msg: any): any;
    receive(fun: Function, timeout?: number, timeoutFn?: () => boolean): (number | symbol | Function | null)[];
    sleep(duration: number | symbol): [symbol, number | symbol];
    suspend(fun: () => any): void;
    delay(fun: () => any, time: number): void;
    schedule(fun: () => any, pid?: PID): void;
    exit(one: PID | any, two?: any): void;
    error(reason: any): void;
    process_flag(...args: any[]): any;
    put(key: string, value: any): void;
    get_process_dict(): object;
    get(key: string, default_value?: any): any;
    get_keys(value: any): string[];
    erase(key: string): void;
    is_alive(pid: any): boolean;
    list(): PID[];
    make_ref(): Reference;
    readonly currentProcess: Process | null;
}
export default ProcessSystem;
