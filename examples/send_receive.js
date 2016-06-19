"use strict";

import { ProcessSystem } from "../src/processes";
self.system = self.system || new ProcessSystem();

let pid = self.system.spawn(function*(){
  var pid1 = self.system.spawn_link(function*(){
    while(true){

      yield self.system.receive(function(value){
        return console.log(value);
      });

      self.system.send(pid2, "message from 1");
    }
  });

  self.system.register("Sally", pid1);


  var pid2 = self.system.spawn_link(function*(){
    while(true){

      self.system.send("Sally", "message from 2");

      yield self.system.receive(function(value){
        return console.log(value);
      });
    }
  });

  yield self.system.receive(function(value){
    return Symbol.for("no_match");
  });
});
