import { PID } from 'erlang-types';
import Mailbox from './mailbox';
import System from './process_system';
/**
 * A Process. Represents the basic atomic level of concurrency in the system
 */
declare class Process {
    pid: PID;
    func: Function;
    args: any[];
    mailbox: Mailbox;
    system: System;
    status: symbol;
    dict: Map<any, any>;
    flags: Map<symbol, any>;
    monitors: any[];
    constructor(system: System, func: Function, args: any[]);
    start(): void;
    main(): IterableIterator<any>;
    process_flag(flag: symbol, value: any): any;
    is_trapping_exits(): boolean;
    signal(reason: any): void;
    receive(fun: Function): symbol;
    run(machine: Generator, step: any): void;
}
export default Process;
