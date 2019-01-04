/* @flow */
'use strict'

import Mailbox from './mailbox'
import Process from './process'
import States from './states'
import Scheduler from './scheduler'
import {PID, Reference, Tuple} from 'erlang-types'

class ProcessSystem {
  pids: Map<PID, Process>
  mailboxes: Map<PID, Mailbox>
  names: Map<any, PID>
  links: Map<PID, Set<PID>>
  monitors: Map<Reference, {monitor: PID; monitee: PID}>
  current_process: Process | null
  scheduler: Scheduler
  suspended: Map<PID, () => any>
  main_process_pid: PID

  constructor() {
    this.pids = new Map()
    this.mailboxes = new Map()
    this.names = new Map()
    this.links = new Map()
    this.monitors = new Map()

    const throttle = 5 //ms between scheduled tasks
    this.current_process = null
    this.scheduler = new Scheduler(throttle)
    this.suspended = new Map()

    let process_system_scope = this
    this.main_process_pid = this.spawn(function*() {
      yield process_system_scope.sleep(Symbol.for('Infinity'))
    })
    this.set_current(this.main_process_pid)
  }

  static *run(
    fun: Function | GeneratorFunction,
    args: any[],
    context: any = null
  ) {
    if (fun.constructor.name === 'GeneratorFunction') {
      return yield* fun.apply(context, args)
    } else {
      return yield fun.apply(context, args)
    }
  }

  spawn(...args: any[]) {
    if (args.length === 1) {
      let fun = args[0]
      return this.add_proc(fun, [], false, false).pid
    } else {
      let mod = args[0]
      let fun = args[1]
      let the_args = args[2]

      return this.add_proc(mod[fun], the_args, false, false).pid
    }
  }

  spawn_link(...args: any[]) {
    if (args.length === 1) {
      let fun = args[0]
      return this.add_proc(fun, [], true, false).pid
    } else {
      let mod = args[0]
      let fun = args[1]
      let the_args = args[2]

      return this.add_proc(mod[fun], the_args, true, false).pid
    }
  }

  link(pid: PID): void {
    const currentProcessPid = this.pid()
    if (currentProcessPid != null) {
      const currentProcessLink = this.links.get(currentProcessPid)
      const IncomingProcessLink = this.links.get(pid)

      if (currentProcessLink && IncomingProcessLink) {
        currentProcessLink.add(pid)
        IncomingProcessLink.add(currentProcessPid)
      }
    }
  }

  unlink(pid: PID): void {
    const currentProcessPid = this.pid()
    if (currentProcessPid != null) {
      const currentProcessLink = this.links.get(currentProcessPid)
      const IncomingProcessLink = this.links.get(pid)

      if (currentProcessLink && IncomingProcessLink) {
        currentProcessLink.delete(pid)
        IncomingProcessLink.delete(currentProcessPid)
      }
    }
  }

  spawn_monitor(...args: any[]) {
    if (args.length === 1) {
      let fun = args[0]
      let process = this.add_proc(fun, [], false, true)
      return [process.pid, process.monitors[0]]
    } else {
      let mod = args[0]
      let fun = args[1]
      let the_args = args[2]
      let process = this.add_proc(mod[fun], the_args, false, true)

      return [process.pid, process.monitors[0]]
    }
  }

  monitor(pid: PID) {
    const real_pid = this.pidof(pid)
    const ref = this.make_ref()

    if (this.currentProcess != null) {
      if (real_pid) {
        this.monitors.set(ref, {
          monitor: this.currentProcess.pid,
          monitee: real_pid,
        })

        const process = this.pids.get(real_pid)
        if (process) {
          process.monitors.push(ref)
        }

        return ref
      } else {
        this.send(
          this.currentProcess.pid,
          new Tuple('DOWN', ref, pid, real_pid, Symbol.for('noproc'))
        )
        return ref
      }
    }
  }

  demonitor(ref: Reference) {
    if (this.monitors.has(ref)) {
      this.monitors.delete(ref)
      return true
    }

    return false
  }

  set_current(id: any) {
    let pid = this.pidof(id)
    if (pid) {
      const next = this.pids.get(pid)
      if (next) {
        this.current_process = next
        if (this.currentProcess) {
          this.currentProcess.status = States.RUNNING
        }
      }
    }
  }

  add_proc(
    fun: GeneratorFunction,
    args: any[],
    linked: boolean,
    monitored: boolean
  ) {
    let newproc = new Process(this, fun, args)

    this.pids.set(newproc.pid, newproc)
    this.mailboxes.set(newproc.pid, newproc.mailbox)
    this.links.set(newproc.pid, new Set())

    if (linked) {
      this.link(newproc.pid)
    }

    if (monitored) {
      this.monitor(newproc.pid)
    }

    newproc.start()
    return newproc
  }

  remove_proc(pid: PID, exitreason: any) {
    this.pids.delete(pid)
    this.unregister(pid)
    this.scheduler.removePid(pid)

    const linkedPids = this.links.get(pid)
    if (linkedPids) {
      for (let linkpid of linkedPids) {
        this.exit(linkpid, exitreason)
        const linkedPid = this.links.get(linkpid)
        if (linkedPid) {
          linkedPid.delete(pid)
        }
      }

      this.links.delete(pid)
    }
  }

  register(name: any, pid: PID) {
    if (!this.names.has(name)) {
      this.names.set(name, pid)
    } else {
      throw new Error('Name is already registered to another process')
    }
  }

  whereis(name: any) {
    return this.names.has(name) ? this.names.get(name) : null
  }

  registered() {
    return this.names.keys()
  }

  unregister(pid: PID) {
    for (let name of this.names.keys()) {
      if (this.names.has(name) && this.names.get(name) === pid) {
        this.names.delete(name)
      }
    }
  }

  pid(): PID | null {
    if (this.currentProcess) {
      return this.currentProcess.pid
    }

    return null
  }

  pidof(id: any) {
    if (id instanceof PID) {
      return this.pids.has(id) ? id : null
    } else if (id instanceof Process) {
      return id.pid
    } else {
      let pid = this.whereis(id)
      if (pid === null)
        throw 'Process name not registered: ' + id + ' (' + typeof id + ')'
      return pid
    }
  }

  send(id: any, msg: any) {
    const pid = this.pidof(id)

    if (pid) {
      const mailbox = this.mailboxes.get(pid)
      if (mailbox) {
        mailbox.deliver(msg)
      }

      if (this.suspended.has(pid)) {
        let fun = this.suspended.get(pid)
        this.suspended.delete(pid)
        if (fun) {
          this.schedule(fun)
        }
      }
    }

    return msg
  }

  receive(fun: Function, timeout = 0, timeoutFn: () => boolean = () => true) {
    let DateTimeout = null

    if (timeout === 0 || timeout === Infinity) {
      DateTimeout = null
    } else {
      DateTimeout = Date.now() + timeout
    }

    return [States.RECEIVE, fun, DateTimeout, timeoutFn]
  }

  sleep(duration: number | symbol): [symbol, number | symbol] {
    return [States.SLEEP, duration]
  }

  suspend(fun: () => any): void {
    if (this.currentProcess) {
      this.currentProcess.status = States.SUSPENDED
      this.suspended.set(this.currentProcess.pid, fun)
    }
  }

  delay(fun: () => any, time: number): void {
    if (this.currentProcess) {
      this.currentProcess.status = States.SLEEPING

      if (Number.isInteger(time)) {
        this.scheduler.scheduleFuture(this.currentProcess.pid, time, fun)
      }
    }
  }

  schedule(fun: () => any, pid?: PID): void {
    if (this.currentProcess) {
      const the_pid = pid != null ? pid : this.currentProcess.pid
      this.scheduler.schedule(the_pid, fun)
    }
  }

  exit(one: PID | any, two?: any): void {
    let pid = null
    let reason = null
    let process = null

    if (two) {
      pid = one
      reason = two
      const thePid = this.pidof(pid)
      if (thePid) {
        process = this.pids.get(thePid)
      }

      if (process) {
        if (
          process.is_trapping_exits() ||
          reason === States.KILL ||
          reason === States.NORMAL
        ) {
          const mailbox = this.mailboxes.get(process.pid)

          if (mailbox) {
            mailbox.deliver(new Tuple(States.EXIT, this.pid(), reason))
          }
        } else {
          process.signal(reason)
        }
      }
    } else {
      if (this.currentProcess) {
        pid = this.currentProcess.pid
        reason = one
        process = this.currentProcess

        process.signal(reason)
      }
    }

    if (process) {
      for (let ref of process.monitors) {
        let mons = this.monitors.get(ref)
        if (mons) {
          this.send(
            mons['monitor'],
            new Tuple('DOWN', ref, mons['monitee'], mons['monitee'], reason)
          )
        }
      }
    }
  }

  error(reason: any): void {
    if (this.currentProcess) {
      this.currentProcess.signal(reason)
    }
  }

  process_flag(...args: any[]): any {
    if (args.length == 2) {
      const flag = args[0]
      const value = args[1]
      if (this.currentProcess) {
        return this.currentProcess.process_flag(flag, value)
      }
    } else {
      const pid = this.pidof(args[0])
      if (pid) {
        const flag = args[1]
        const value = args[2]
        const process = this.pids.get(pid)

        if (process) {
          return process.process_flag(flag, value)
        }
      }
    }
  }

  put(key: string, value: any): void {
    if (this.currentProcess) {
      this.currentProcess.dict.set(key, value)
    }
  }

  get_process_dict(): object {
    if (this.currentProcess) {
      return this.currentProcess.dict
    }

    throw new Error('No Current Process')
  }

  get(key: string, default_value: any = null) {
    if (this.currentProcess && key in this.currentProcess.dict) {
      return this.currentProcess.dict.get(key)
    } else {
      return default_value
    }
  }

  get_keys(value: any): string[] {
    if (value) {
      let keys = []

      if (this.currentProcess) {
        for (let key of Object.keys(this.currentProcess.dict)) {
          if (this.currentProcess.dict.get(key) === value) {
            keys.push(key)
          }
        }
      }

      return keys
    }

    if (this.currentProcess) {
      return Object.keys(this.currentProcess.dict)
    }

    throw new Error('No Current Process')
  }

  erase(key: string): void {
    if (this.currentProcess) {
      if (key != null && this.currentProcess.dict.has(key)) {
        this.currentProcess.dict.delete(key)
      } else {
        this.currentProcess.dict = new Map()
      }
    }
  }

  is_alive(pid: any) {
    const real_pid = this.pidof(pid)
    return real_pid != null
  }

  list(): PID[] {
    return Array.from(this.pids.keys())
  }

  make_ref(): Reference {
    return new Reference()
  }

  get currentProcess(): Process | null {
    if (this.current_process) {
      return this.current_process
    }

    return null
  }
}

export default ProcessSystem
