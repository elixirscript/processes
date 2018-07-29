import {PID} from 'erlang-types'
import ProcessQueue from './process_queue'

class Scheduler {
  isRunning: boolean
  invokeLater: (callback: () => void) => void
  reductions_per_process: number
  queues: Map<PID, ProcessQueue>
  constructor(throttle: number = 0, reductions_per_process: number = 8) {
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

  addToQueue(pid: PID, task: () => any) {
    if (!this.queues.has(pid)) {
      this.queues.set(pid, new ProcessQueue(pid))
    }

    const queue = this.queues.get(pid)
    if (queue) {
      queue.add(task)
    }
  }

  removePid(pid: PID) {
    this.isRunning = true

    this.queues.delete(pid)

    this.isRunning = false
  }

  run() {
    if (this.isRunning) {
      this.invokeLater(() => {
        this.run()
      })
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
            if (task) {
              result = task()
            }
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

      this.invokeLater(() => {
        this.run()
      })
    }
  }

  addToScheduler(pid: PID, task: () => any, dueTime: number = 0) {
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

  schedule(pid: PID, task: () => any) {
    this.addToScheduler(pid, () => {
      task()
    })
  }

  scheduleFuture(pid: PID, dueTime: number, task: () => any) {
    this.addToScheduler(
      pid,
      () => {
        task()
      },
      dueTime
    )
  }
}

export default Scheduler
