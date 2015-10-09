"use strict";

import ProcessManager from "./process_manager";

let manager = new ProcessManager();


let pid1 = manager.spawn(function*(){
    while(true){

      yield manager.receive(function(value){
        return console.log(value);
      });

      manager.send(2, "message from 1");
    }
});

manager.register("Sally", pid1);


let pid2 = manager.spawn(function*(){
  while(true){
    
    manager.send("Sally", "message from 2");

    yield manager.receive(function(value){
      return console.log(value);
    });
  }
});


let pid3 = manager.spawn(function*(){
  yield 1;
});