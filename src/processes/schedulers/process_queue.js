'use strict'

export default class ProcessQueue {
  constructor(pid) {
    this.pid = pid
    this.tasks = []
  }

  empty() {
    return this.tasks.length === 0
  }

  add(task) {
    this.tasks.push(task)
  }

  next() {
    return this.tasks.shift()
  }
}
