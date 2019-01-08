import DefaultScheduler from './default_scheduler';
/**
 * Scheduler for the process system.
 * Uses window.requestAnimationFrame to schedule process execution
 * Good for processes that do a lot of dom manipulation
 */
declare class RequestAnimationScheduler extends DefaultScheduler {
    constructor(throttle?: number, reductions_per_process?: number);
    _run(run: FrameRequestCallback): void;
}
export default RequestAnimationScheduler;
