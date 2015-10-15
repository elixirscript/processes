"use strict";

import { Scheduler, GenServer } from "../src/processes";
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
  console.log(a);

  let b = yield* Scheduler.run(GenServer.cast, [pid, ["push", "world"]]);
  console.log(b);

  let c = yield* Scheduler.run(GenServer.call, [pid, "pop"]);
  console.log(c);
});