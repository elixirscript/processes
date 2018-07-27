/* @flow */
'use strict'

import Mailbox from './mailbox'
import Process from './process'
import States from './states'
import Scheduler from './scheduler'
import ErlangTypes from 'erlang-types'

class ProcessSystem {
  pids: Map<ErlangTypes.PID, Process>
  mailboxes: Map<ErlangTypes.PID, Mailbox>
  names: Map<any, ErlangTypes.PID>
  links: Map<ErlangTypes.PID, Set<ErlangTypes.PID>>
  monitors: Map<
    ErlangTypes.Reference,
    {monitor: ErlangTypes.PID; monitee: ErlangTypes.PID}
  >
  current_process: Process | null
  scheduler: Scheduler
  suspended: Map<ErlangTypes.PID, Function>
  main_process_pid: Process

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
      return this.add_proc(fun, [], false).pid
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

  link(pid: ErlangTypes.PID) {
    this.links.get(this.pid()).add(pid)
    this.links.get(pid).add(this.pid())
  }

  unlink(pid: ErlangTypes.PID) {
    this.links.get(this.pid()).delete(pid)
    this.links.get(pid).delete(this.pid())
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

  monitor(pid: ErlangTypes.PID) {
    const real_pid = this.pidof(pid)
    const ref = this.make_ref()

    if (real_pid) {
      this.monitors.set(ref, {
        monitor: this.current_process.pid,
        monitee: real_pid,
      })
      this.pids.get(real_pid).monitors.push(ref)
      return ref
    } else {
      this.send(
        this.current_process.pid,
        new ErlangTypes.Tuple('DOWN', ref, pid, real_pid, Symbol.for('noproc'))
      )
      return ref
    }
  }

  demonitor(ref: ErlangTypes.Reference) {
    if (this.monitors.has(ref)) {
      this.monitors.delete(ref)
      return true
    }

    return false
  }

  set_current(id: any) {
    let pid = this.pidof(id)
    if (pid !== null) {
      const next = this.pids.get(pid)
      if (next) {
        this.current_process = next
        this.current_process.status = States.RUNNING
      }
    }
  }

  add_proc(
    fun: GeneratorFunction,
    args: any[],
    linked: boolean,
    monitored: boolean
  ) {
    let newpid = new ErlangTypes.PID()
    let mailbox = new Mailbox()
    let newproc = new Process(newpid, fun, args, mailbox, this)

    this.pids.set(newpid, newproc)
    this.mailboxes.set(newpid, mailbox)
    this.links.set(newpid, new Set())

    if (linked) {
      this.link(newpid)
    }

    if (monitored) {
      this.monitor(newpid)
    }

    newproc.start()
    return newproc
  }

  remove_proc(pid: ErlangTypes.PID, exitreason: any) {
    this.pids.delete(pid)
    this.unregister(pid)
    this.scheduler.removePid(pid)

    if (this.links.has(pid)) {
      for (let linkpid of this.links.get(pid)) {
        this.exit(linkpid, exitreason)
        this.links.get(linkpid).delete(pid)
      }

      this.links.delete(pid)
    }
  }

  register(name: any, pid: ErlangTypes.PID) {
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

  unregister(pid: ErlangTypes.PID) {
    for (let name of this.names.keys()) {
      if (this.names.has(name) && this.names.get(name) === pid) {
        this.names.delete(name)
      }
    }
  }

  pid() {
    return this.current_process.pid
  }

  pidof(id: any) {
    if (id instanceof ErlangTypes.PID) {
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
      this.mailboxes.get(pid).deliver(msg)

      if (this.suspended.has(pid)) {
        let fun = this.suspended.get(pid)
        this.suspended.delete(pid)
        this.schedule(fun)
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

  sleep(duration: number | Symbol): [Symbol, number | Symbol] {
    return [States.SLEEP, duration]
  }

  suspend(fun: Function): void {
    if (this.current_process) {
      this.current_process.status = States.SUSPENDED
      this.suspended.set(this.current_process.pid, fun)
    }
  }

  delay(fun: Function, time: number): void {
    if (this.current_process) {
      this.current_process.status = States.SLEEPING

      if (Number.isInteger(time)) {
        this.scheduler.scheduleFuture(this.current_process.pid, time, fun)
      }
    }
  }

  schedule(fun: () => any, pid?: ErlangTypes.PID): void {
    if (this.current_process) {
      const the_pid = pid != null ? pid : this.current_process.pid
      this.scheduler.schedule(the_pid, fun)
    }
  }

  exit(one: ErlangTypes.PID | any, two?: any): void {
    let pid = null
    let reason = null
    let process = null

    if (two) {
      pid = one
      reason = two
      process = this.pids.get(this.pidof(pid))

      if (
        (process && process.is_trapping_exits()) ||
        reason === States.KILL ||
        reason === States.NORMAL
      ) {
        this.mailboxes
          .get(process.pid)
          .deliver(new ErlangTypes.Tuple(States.EXIT, this.pid(), reason))
      } else {
        process.signal(reason)
      }
    } else {
      if (this.current_process) {
        pid = this.current_process.pid
        reason = one
        process = this.current_process

        process.signal(reason)
      }
    }

    if (process) {
      for (let ref in process.monitors) {
        let mons = this.monitors.get(ref)
        if (mons) {
          this.send(
            mons['monitor'],
            new ErlangTypes.Tuple(
              'DOWN',
              ref,
              mons['monitee'],
              mons['monitee'],
              reason
            )
          )
        }
      }
    }
  }

  error(reason: any): void {
    if (this.current_process) {
      this.current_process.signal(reason)
    }
  }

  process_flag(...args: any[]) {
    if (args.length == 2) {
      const flag = args[0]
      const value = args[1]
      return this.current_process.process_flag(flag, value)
    } else {
      const pid = this.pidof(args[0])
      const flag = args[1]
      const value = args[2]
      return this.pids.get(pid).process_flag(flag, value)
    }
  }

  put(key: string, value: any): void {
    this.current_process.dict[key] = value
  }

  get_process_dict(): Object {
    return this.current_process.dict
  }

  get(key: string, default_value = null) {
    if (key in this.current_process.dict) {
      return this.current_process.dict[key]
    } else {
      return default_value
    }
  }

  get_keys(value: any) {
    if (value) {
      let keys = []

      for (let key of Object.keys(this.current_process.dict)) {
        if (this.current_process.dict[key] === value) {
          keys.push(key)
        }
      }

      return keys
    }

    return Object.keys(this.current_process.dict)
  }

  erase(key: string): void {
    if (key != null) {
      delete this.current_process.dict[key]
    } else {
      this.current_process.dict = {}
    }
  }

  is_alive(pid: any) {
    const real_pid = this.pidof(pid)
    return real_pid != null
  }

  list(): ErlangTypes.PID[] {
    return Array.from(this.pids.keys())
  }

  make_ref(): ErlangTypes.Reference {
    return new ErlangTypes.Reference()
  }
}

export default ProcessSystem
