import {PID} from 'erlang-types'
import States from './states'
import Mailbox from './mailbox'
import System from './process_system'

function is_sleep(value: any) {
  return Array.isArray(value) && value[0] === States.SLEEP
}

function is_receive(value: any) {
  return Array.isArray(value) && value[0] === States.RECEIVE
}

function receive_timed_out(value: any) {
  return value[2] != null && value[2] < Date.now()
}

class Process {
  pid: PID
  func: Function
  args: any[]
  mailbox: Mailbox
  system: System
  status: Symbol
  dict: Object
  flags: Object
  monitors: any[]

  constructor(
    pid: PID,
    func: Function,
    args: any[],
    mailbox: Mailbox,
    system: System
  ) {
    this.pid = pid
    this.func = func
    this.args = args
    this.mailbox = mailbox
    this.system = system
    this.status = States.STOPPED
    this.dict = {}
    this.flags = {}
    this.monitors = []
  }

  start() {
    const function_scope = this
    let machine = this.main()

    this.system.schedule(function() {
      function_scope.system.set_current(function_scope.pid)
      function_scope.run(machine, machine.next())
    }, this.pid)
  }

  *main() {
    let retval = States.NORMAL

    try {
      yield* this.func.apply(null, this.args)
    } catch (e) {
      console.error(e)
      retval = e
    }

    this.system.exit(retval)
  }

  process_flag(flag, value) {
    const old_value = this.flags[flag]
    this.flags[flag] = value
    return old_value
  }

  is_trapping_exits(): boolean {
    return (
      this.flags[Symbol.for('trap_exit')] &&
      this.flags[Symbol.for('trap_exit')] == true
    )
  }

  signal(reason: any): void {
    if (reason !== States.NORMAL) {
      console.error(reason)
    }

    this.system.remove_proc(this.pid, reason)
  }

  receive(fun: Function) {
    let value = States.NOMATCH
    let messages = this.mailbox.get()

    for (let i = 0; i < messages.length; i++) {
      try {
        value = fun(messages[i])
        if (value !== States.NOMATCH) {
          this.mailbox.removeAt(i)
          break
        }
      } catch (e) {
        if (e.constructor.name != 'MatchError') {
          this.system.exit(e)
        }
      }
    }

    return value
  }

  run(machine: Generator, step: any): void {
    const function_scope = this

    if (!step.done) {
      let value = step.value

      if (is_sleep(value)) {
        this.system.delay(function() {
          function_scope.system.set_current(function_scope.pid)
          function_scope.run(machine, machine.next())
        }, value[1])
      } else if (is_receive(value) && receive_timed_out(value)) {
        let result = value[3]()

        this.system.schedule(function() {
          function_scope.system.set_current(function_scope.pid)
          function_scope.run(machine, machine.next(result))
        })
      } else if (is_receive(value)) {
        let result = function_scope.receive(value[1])

        if (result === States.NOMATCH) {
          this.system.suspend(function() {
            function_scope.system.set_current(function_scope.pid)
            function_scope.run(machine, step)
          })
        } else {
          this.system.schedule(function() {
            function_scope.system.set_current(function_scope.pid)
            function_scope.run(machine, machine.next(result))
          })
        }
      } else {
        this.system.schedule(function() {
          function_scope.system.set_current(function_scope.pid)
          function_scope.run(machine, machine.next(value))
        })
      }
    }
  }
}

export default Process
