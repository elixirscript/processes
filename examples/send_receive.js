"use strict";

import { Scheduler, GenServer } from "../src/processes.js";
self.scheduler = self.scheduler || new Scheduler();


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