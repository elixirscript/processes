Experiment to reproduce erlang style processes in browser. The api follows the one from Erlang. All are found on the `Scheduler` class
    

### Running Examples

* `jspm install`
* `python -m SimpleHTTPServer 8000`

Going to `localhost:8000` will show links to examples.

One example is an implementation of a GenServer. The other example is 2 processes talking
to each other.

#### Usage

* First, import the Scheduler create a new instance of one.
  ```javascript
    import { Scheduler } from "processes";
    let scheduler = new Scheduler();
  ```
  
* Now you can spawn processes using the scheduler. 

    A process will switch to other processes when yield is used and will run until it completes.
    
    ```javascript
    var pid1 = scheduler.spawn(function*(){
        while(true){
    
          yield scheduler.receive(function(value){
            return console.log(value);
          });
    
          scheduler.send(pid2, "message from 1");
        }
    });
    
    scheduler.register("Sally", pid1);
    
    var pid2 = scheduler.spawn(function*(){
      while(true){
        scheduler.send("Sally", "message from 2");
    
        yield scheduler.receive(function(value){
          return console.log(value);
        });
      }
    });
    
    ```

### API

* Scheduler
    * `spawn(fun*) : pid` - Starts a process represented by the given generator function
    * `spawn(module, fun, args) : pid` - Starts a process using the generator function from the specified module
    * `link(pid) : void` - links the current process with the process from the given pid
    * `unlink(pid) : void` - unlinks the current process from the process from the given pid
    * `register(name, pid) : void` - registers the given name to the pid
    * `registered(name) : pid` - returns the pid registered by the given name or null if not registered
    * `unregister(pid) : void` - unregisters the names associated with the pid
    * `pid()` : pid` - returns the current process's pid
    * `pidof(obj) : pid` - takes the input and tries to find the pid. Input can be a `pid`, `Process`, or name the pid is associated with
    * `send(pid, msg) : msg` - sends a message the the process represented by the pid
    * `receive(fun, timeout = 0, timeoutFn = () => true)` - Tells the current process to receive a message that the function can handle. If no match then the process is put in the suspended state until a message arrives or the timeout is reached. If the timeout is reached and no msg matches, then the timeoutFn is called
    * `sleep(duration)` - puts the current process to sleep
    * `exit(reason)` - terminates the current process with the given reason.
    * `exit(pid, reason)` - tells the process with the pid to exit with the given reason
    * `error(reason)` - terminates the current process with an error
    * `process_flag(flag, value)` - Sets flags on the current process.
        * Note: the only flag respected is the `Symbol.for("trap_exit")` flag. If value is `true`, then exit signals from linked processes are turned into messages and sent to the current processes mailbox. If value is `false`, the exit is treated as normal and terminates the process. Setting it to `true` is useful for supervising processes.
    * `put(key, value)` - Adds a value to the current process's dictionary
    * `get(key)` - Gets a value from the current process's dictionary
    * `get()` - Gets the current process's dictionary
    * `get_keys()` - Gets all the keys from the current process's dictionary
    * `erase(key)` - Removes the key and the associated value from the current process`s dictionary
    * `erase()` - Removes all entries from the current process's dictionary

* `Scheduler.run(fun, args, context = null)` - A static generator function used to wrap a normal function or generator. If fun is a function, it returns the value, if it's a generator, then it delegates yielding to the generator.

* GenServer
    * `start(module, args)` - Starts a GenServer with the given module and args
    * `start_link(module, args)` - Starts a GenServer with the given module and args
    * `call* (server, action)` - Sends the GenServer a action and waits for it to respond with a value.
    * `cast* (server, action)` - Sends the GenServer a action to update a value.
    * `stop (server)` - Stops the GenServer.
    * **Note**: Genserver expects a module the has the following functions:
        * `init(args)` - Must return an array containing a symbol and the initial state
        * `handle_call(action, from, state)` - Called when `GenServer.call` is called. This function is given the action, the pid of the calling process, and the current state. Must return `[reply, return_value, new_state]` where reply is a symbol ,usually `Symbol.for("reply"), the value to return to the process, and lastly, the new state of the GenServer.
        * `handle_cast(action, state)` - Called when `GenServer.cast` is called. his function is given the action, and the current state. Must return `[reply, return_value, new_state]` where reply is a symbol ,usually `Symbol.for("noreply")`, and lastly, the new state of the GenServer.
    
#### GenServer Example

An example of a Stack using a GenServer

```javascript
import { Scheduler, GenServer } from "processes";
self.scheduler = self.scheduler || new Scheduler();

const Stack = {
  init: function(args){
    return [Symbol.for("ok"), args];
  },

  handle_call: function(action, pid, state){
    return [Symbol.for("reply"), state[0], state.slice(1)];
  },


  handle_cast: function(action, state){
    return [Symbol.for("noreply"), [action[1]].concat(state)];
  }
}

self.scheduler.spawn(function*(){
  const [ok, pid] = yield* Scheduler.run(GenServer.start, [Stack, ["hello"]]);

  let a = yield* Scheduler.run(GenServer.call, [pid, "pop"]);
  console.log(a); // "hello"

  let b = yield* Scheduler.run(GenServer.cast, [pid, ["push", "world"]]);
  console.log(b); // Symbol.for("ok")

  let c = yield* Scheduler.run(GenServer.call, [pid, "pop"]);
  console.log(c); // "world"
});
```