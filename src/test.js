"use strict";

import Scheduler, { SLEEP, RECEIVE, SEND } from "./scheduler";

self.s = new Scheduler();
self.SLEEP = SLEEP;
self.SEND = SEND;
self.RECEIVE = RECEIVE;


let pid1 = s.spawn(function*(){
    while(true){

      yield [RECEIVE, function(value){
        return console.log(value);
      }]

      s.send(2, "message from 1");
    }
});

s.register("Sally", pid1);


let pid2 = s.spawn(function*(){
  while(true){
    
    s.send("Sally", "message from 2");

    yield [RECEIVE, function(value){
        return console.log(value);
    }]
  }
});


let pid3 = s.spawn(function*(){
  yield 1;
});