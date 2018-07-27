[![Build Status](https://travis-ci.org/elixirscript/processes.svg?branch=master)](https://travis-ci.org/elixirscript/processes)

Experiment to reproduce Erlang style processes in browser. The api follows the one from Erlang. All are found on the `ProcessSystem` class

#### Usage

- First, import the ProcessSystem create a new instance of one.

  ```javascript
  const Processes = require('erlang-processes')
  let system = new Processes.default.ProcessSystem()
  ```

- Now you can spawn processes using the system.

  A process will switch to other processes when yield is used and will run until it completes.

  ```javascript
  var pid1 = system.spawn(function*() {
    while (true) {
      yield system.receive(function(value) {
        return console.log(value)
      })

      system.send(pid2, 'message from 1')
    }
  })

  system.register('Sally', pid1)

  var pid2 = system.spawn(function*() {
    while (true) {
      system.send('Sally', 'message from 2')

      yield system.receive(function(value) {
        return console.log(value)
      })
    }
  })
  ```

### API

- ProcessSystem

  - `spawn(fun*) : pid` - Starts a process represented by the given generator function
  - `spawn(module, fun, args) : pid` - Starts a process using the generator function from the specified module
  - `link(pid) : void` - links the current process with the process from the given pid
  - `unlink(pid) : void` - unlinks the current process from the process from the given pid
  - `register(name, pid) : void` - registers the given name to the pid
  - `whereis(name) : pid` - returns the pid registered by the given name or null if not registered
  - `unregister(pid) : void` - unregisters the names associated with the pid
  - `registered() : Array` - returns the liast of names that are registered
  - `pid()` : pid` - returns the current process's pid
  - `pidof(obj) : pid` - takes the input and tries to find the pid. Input can be a `pid`, `Process`, or name the pid is associated with
  - `send(pid, msg) : msg` - sends a message the the process represented by the pid
  - `receive(fun, timeout = 0, timeoutFn = () => true)` - Tells the current process to receive a message that the function can handle. If no match then the process is put in the suspended state until a message arrives or the timeout is reached. If the timeout is reached and no msg matches, then the timeoutFn is called
  - `sleep(duration)` - puts the current process to sleep
  - `exit(reason)` - terminates the current process with the given reason.
  - `exit(pid, reason)` - tells the process with the pid to exit with the given reason
  - `error(reason)` - terminates the current process with an error
  - `process_flag(pid, flag, value)` - Sets flags on the given process.
  - `process_flag(flag, value)` - Sets flags on the current process.
    - Note: the only flag respected is the `Symbol.for("trap_exit")` flag. If value is `true`, then exit signals from linked processes are turned into messages and sent to the current processes mailbox. If value is `false`, the exit is treated as normal and terminates the process. Setting it to `true` is useful for supervising processes.
  - `put(key, value)` - Adds a value to the current process's dictionary
  - `get(key, default_value = null)` - Gets a value from the current process's dictionary or the default if key not in dictionary
  - `get_process_dict()` - Gets the current process's dictionary
  - `get_keys()` - Gets all the keys from the current process's dictionary
  - `get_keys(value)` - Gets all the keys from the current process's dictionary with the given value
  - `erase(key)` - Removes the key and the associated value from the current process's dictionary
  - `erase()` - Removes all entries from the current process's dictionary
  - `is_alive(pid)` - Returns if the given pid is alive
  - `make_ref()` - Returns a unique reference
  - `list()` - Returns a list of all the pids
  - `monitor(pid)` - Monitors the given process
  - `demonitor(ref)` - Removes the monitor

- `ProcessSystem.run(fun, args, context = null)` - A static generator function used to wrap a normal function or generator. If fun is a function, it returns the value, if it's a generator, then it delegates yielding to the generator.

## References

- [Er.js](https://github.com/orph/erjs)
- [Erlang Processes](http://erlang.org/doc/reference_manual/processes.html)
- [ES6 Generators Deliver Go Style Concurrency](http://swannodette.github.io/2013/08/24/es6-generators-and-csp)
- [Red and Green Callbacks](http://joearms.github.io/2013/04/02/Red-and-Green-Callbacks.html)
