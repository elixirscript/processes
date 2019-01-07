import DefaultScheduler from './default_scheduler';
declare class RequestAnimationScheduler extends DefaultScheduler {
    constructor(throttle?: number, reductions_per_process?: number);
    _run(run: FrameRequestCallback): void;
}
export default RequestAnimationScheduler;
