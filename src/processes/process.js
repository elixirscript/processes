"use strict";

/* @flow */
import Mailbox from "./mailbox";
import Scheduler from "./scheduler";
import States from "./states";

const NOMSG = Symbol();

class Process {
  pid: Number;
  mailbox: Mailbox;
  func: Function;
  args: Array;
  scheduler: Scheduler;
  status: Symbol;

  constructor(pid: Number, func: Function, args: Array, mailbox: Mailbox, scheduler: Scheduler){
    this.pid = pid;
    this.func = func;
    this.args = args;
    this.mailbox = mailbox;
    this.scheduler = scheduler;
    this.status = States.STOPPED;
    this.dictionary = {};
  }

  start(){
    const function_scope = this;
    let machine = this.main();

    this.scheduler.queue(function() {
      function_scope.scheduler.set_current(function_scope.pid); 
      function_scope.run(machine, machine.next()); 
    }, this.pid);  
  }

  *main() {
    let retval = States.NORMAL;

    try {
      yield* this.func.apply(null, this.args);
    } catch(e) {
      retval = e;
    }

    this.scheduler.exit(this.pid, retval);
  }

  exit(reason){
    this.scheduler.remove_proc(this.pid, reason);
  }

  receive(fun){
    let value = NOMSG;
    let messages = this.mailbox.get();

    for(let i = 0; i < messages.length; i++){
      try{
        value = fun(messages[i]);
        this.mailbox.removeAt(i);
      }catch(e){
        this.exit(e);
      }
    }

    return value;
  }

  run(machine, step){
    const function_scope = this;
    
    if(!step.done){
      let value = step.value;

      if(Array.isArray(value) && (value[0] === States.SLEEP || value[0] === States.RECEIVE)){
        if(value[0] === States.SLEEP){

          this.scheduler.delay(function() {
            function_scope.scheduler.set_current(function_scope.pid); 
            function_scope.run(machine, machine.next()); 
          }, value[1]);

        }else if(value[0] === States.RECEIVE){
          if(value[2] != null && value[2] < Date.now()){
            let result = value[3]();

            this.scheduler.queue(function() {
              function_scope.scheduler.set_current(function_scope.pid); 
              function_scope.run(machine, machine.next(result)); 
            });
          }else{
            let result = function_scope.receive(value[1]);

            if(result === NOMSG){
              this.scheduler.suspend(function() { 
                function_scope.scheduler.set_current(function_scope.pid); 
                function_scope.run(machine, step); 
              });         
            }else{
              this.scheduler.queue(function() { 
                function_scope.scheduler.set_current(function_scope.pid); 
                function_scope.run(machine, machine.next(result)); 
              });          
            }
          }
        }      
      }else{
        this.scheduler.queue(function() {
          function_scope.scheduler.set_current(function_scope.pid); 
          function_scope.run(machine, machine.next(value)); 
        });  
      }
    }
  }
}

export default Process;