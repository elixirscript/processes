import { PID } from 'erlang-types';
declare class ProcessQueue {
    pid: PID;
    tasks: Function[];
    constructor(pid: PID);
    empty(): boolean;
    add(task: any): void;
    next(): Function | undefined;
}
export default ProcessQueue;
