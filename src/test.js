"use strict";

import Scheduler from "./scheduler";
let scheduler = new Scheduler();

let pid1 = scheduler.spawn(function*(){
    while(true){

      yield scheduler.receive(function(value){
        return console.log(value);
      });

      scheduler.send(2, "message from 1");
    }
});

scheduler.register("Sally", pid1);


let pid2 = scheduler.spawn(function*(){
  while(true){
    
    scheduler.send("Sally", "message from 2");

    yield scheduler.receive(function(value){
      return console.log(value);
    });
  }
});


let pid3 = scheduler.spawn(function*(){
  yield 1;
});