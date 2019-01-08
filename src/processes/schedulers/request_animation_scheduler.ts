import DefaultScheduler from './default_scheduler'

/**
 * Scheduler for the process system.
 * Uses window.requestAnimationFrame to schedule process execution
 * Good for processes that do a lot of dom manipulation
 */
class RequestAnimationScheduler extends DefaultScheduler {
  constructor(throttle = 0, reductions_per_process = 8) {
    super(throttle, reductions_per_process)
  }

  _run(run: FrameRequestCallback) {
    window.requestAnimationFrame(run)
  }
}

export default RequestAnimationScheduler
