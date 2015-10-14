"use strict";

import Scheduler from "./processes/scheduler";
self.scheduler = self.scheduler || new Scheduler();

import GenServer from "./processes/otp/gen_server";

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
  const [ok, pid] = yield* Scheduler.async(GenServer.start, [Stack, ["hello"]]);

  let a = yield* Scheduler.async(GenServer.call, [pid, "pop"]);
  console.log(a);

  let b = yield* Scheduler.async(GenServer.cast, [pid, ["push", "world"]]);
  console.log(b);

  let c = yield* Scheduler.async(GenServer.call, [pid, "pop"]);
  console.log(c);
});


//var pid1 = scheduler.spawn(function*(){
//    while(true){

//      yield scheduler.receive(function(value){
//        return console.log(value);
//      });

//      scheduler.send(pid2, "message from 1");
//    }
//});

//scheduler.register("Sally", pid1);
//

//var pid2 = scheduler.spawn(function*(){
//  while(true){
//    
//    scheduler.send("Sally", "message from 2");

//    yield scheduler.receive(function(value){
//      return console.log(value);
//    });
//  }
//});