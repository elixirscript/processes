"use strict";

import { ProcessSystem, GenServer } from "../src/processes";
self.system = self.system || new ProcessSystem();


var pid1 = system.spawn(function*(){
    while(true){

      yield system.receive(function(value){
        return console.log(value);
      });

      system.send(pid2, "message from 1");
    }
});

system.register("Sally", pid1);


var pid2 = system.spawn(function*(){
  while(true){
    
    system.send("Sally", "message from 2");

    yield system.receive(function(value){
      return console.log(value);
    });
  }
});