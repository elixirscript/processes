"use strict";

import { ProcessSystem, GenServer } from "../src/processes";
self.system = self.system || new ProcessSystem();

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

self.system.spawn(function*(){
  const [ok, pid] = yield* ProcessSystem.run(GenServer.start, [Stack, ["hello"]]);

  let a = yield* ProcessSystem.run(GenServer.call, [pid, "pop"]);
  console.log(a);

  let b = yield* ProcessSystem.run(GenServer.cast, [pid, ["push", "world"]]);
  console.log(b);

  let c = yield* ProcessSystem.run(GenServer.call, [pid, "pop"]);
  console.log(c);
});