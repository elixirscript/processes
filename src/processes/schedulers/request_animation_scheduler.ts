import DefaultScheduler from './default_scheduler'

class RequestAnimationScheduler extends DefaultScheduler {
  constructor(throttle = 0, reductions_per_process = 8) {
    super(throttle, reductions_per_process)
  }

  _run(run: FrameRequestCallback) {
    window.requestAnimationFrame(run)
  }
}

export default RequestAnimationScheduler
