"use strict";

/* @flow */
import Mailbox from "./mailbox";
import Scheduler from "./scheduler";
import States from "./states";

const NOMSG = Symbol();

function is_sleep(value){
  return Array.isArray(value) && value[0] === States.SLEEP;
}

function is_receive(value){
  return Array.isArray(value) && value[0] === States.RECEIVE;
}

function receive_timed_out(value){
  return value[2] != null && value[2] < Date.now();
}

class Process {
  pid: Number;
  mailbox: Mailbox;
  func: Function;
  args: Array;
  scheduler: Scheduler;
  status: Symbol;
  dict: Object;
  flags: Object;

  constructor(pid: Number, func: Function, args: Array, mailbox: Mailbox, scheduler: Scheduler){
    this.pid = pid;
    this.func = func;
    this.args = args;
    this.mailbox = mailbox;
    this.scheduler = scheduler;
    this.status = States.STOPPED;
    this.dict = {};
    this.flags = {};
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
      console.error(e);
      retval = e;
    }

    this.scheduler.exit(retval);
  }

  process_flag(flag, value){
    this.flags[flag] = value;
  }

  is_trapping_exits(){
    return this.flags[Symbol.for("trap_exit")] && this.flags[Symbol.for("trap_exit")] == true;
  }

  signal(reason){
    if(reason !== States.NORMAL){
      console.error(reason);
    }

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

      if(is_sleep(value)){

        this.scheduler.delay(function() {
          function_scope.scheduler.set_current(function_scope.pid); 
          function_scope.run(machine, machine.next()); 
        }, value[1]);

      }else if(is_receive(value) && receive_timed_out(value)){

        let result = value[3]();

        this.scheduler.queue(function() { 
          function_scope.scheduler.set_current(function_scope.pid); 
          function_scope.run(machine, machine.next(result)); 
        });

      }else if(is_receive(value)){

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