import {PID} from 'erlang-types'

class ProcessQueue {
  pid: PID
  tasks: Function[]

  constructor(pid: PID) {
    this.pid = pid
    this.tasks = []
  }

  empty() {
    return this.tasks.length === 0
  }

  add(task: any) {
    this.tasks.push(task)
  }

  next() {
    return this.tasks.shift()
  }
}

export default ProcessQueue
