import { PID } from 'erlang-types';
import ProcessQueue from './process_queue';
/**
 * Default scheduler for the process system.
 * Schedules process execution using setTimeout.
 * The most generic scheduler and maybe not good for
 * anything with dom manipulation.
 */
declare class DefaultScheduler {
    isRunning: boolean;
    invokeLater: (callback: () => void) => void;
    reductions_per_process: number;
    queues: Map<PID, ProcessQueue>;
    constructor(throttle?: number, reductions_per_process?: number);
    addToQueue(pid: PID, task: () => any): void;
    removePid(pid: PID): void;
    _run(run: Function): void;
    run(): void;
    addToScheduler(pid: PID, task: () => any, dueTime?: number): void;
    schedule(pid: PID, task: () => any): void;
    scheduleFuture(pid: PID, dueTime: number, task: () => any): void;
}
export default DefaultScheduler;
