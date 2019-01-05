import ProcessQueue from './process_queue'

class DefaultScheduler {
  constructor(throttle = 0, reductions_per_process = 8) {
    this.isRunning = false
    this.invokeLater = function(callback) {
      setTimeout(callback, throttle)
    }

    // In our case a reduction is equal to a task call
    // Controls how many tasks are called at a time per process
    this.reductions_per_process = reductions_per_process
    this.queues = new Map()
    this.run()
  }

  addToQueue(pid, task) {
    if (!this.queues.has(pid)) {
      this.queues.set(pid, new ProcessQueue(pid))
    }

    this.queues.get(pid).add(task)
  }

  removePid(pid) {
    this.isRunning = true

    this.queues.delete(pid)

    this.isRunning = false
  }

  _run(run) {
    this.invokeLater(() => {
      run()
    })
  }

  run() {
    if (this.isRunning) {
      this._run(this.run.bind(this))
    } else {
      for (let [pid, queue] of this.queues) {
        let reductions = 0
        while (
          queue &&
          !queue.empty() &&
          reductions < this.reductions_per_process
        ) {
          let task = queue.next()
          this.isRunning = true

          let result

          try {
            result = task()
          } catch (e) {
            console.error(e)
            result = e
          }

          this.isRunning = false

          if (result instanceof Error) {
            throw result
          }

          reductions++
        }
      }

      this._run(this.run.bind(this))
    }
  }

  addToScheduler(pid, task, dueTime = 0) {
    if (dueTime === 0) {
      this.invokeLater(() => {
        this.addToQueue(pid, task)
      })
    } else {
      setTimeout(() => {
        this.addToQueue(pid, task)
      }, dueTime)
    }
  }

  schedule(pid, task) {
    this.addToScheduler(pid, () => {
      task()
    })
  }

  scheduleFuture(pid, dueTime, task) {
    this.addToScheduler(
      pid,
      () => {
        task()
      },
      dueTime
    )
  }
}

export default DefaultScheduler
