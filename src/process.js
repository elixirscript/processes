"use strict";

/* @flow */
import Mailbox from "./mailbox";
import Scheduler, { NORMAL, SUSPEND, SLEEP, CONTINUE, SLEEPING, RUNNING, SUSPENDED, STOPPED, RECEIVE, SEND } from "./scheduler";

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
    this.status = STOPPED;
  }

  start(){
    let machine = this.main();
    let step = machine.next();

    this.status = RUNNING;
    
    this.run(machine, step);
  }

  *main() {
    let retval = NORMAL;

    try {
      for(let v of this.func.apply(null, this.args)){
        yield v;
      }
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
        if(!e instanceof MatchError){
          this.exit(e);
        }
      }
    }

    return value;
  }

  run(machine, step){
    const function_scope = this;
    this.scheduler.set_current(this);

    if(!step.done){
      let value = step.value;

      if(typeof value === 'symbol'){
        switch(value){
          case SUSPEND:
            this.scheduler.suspend(function() { 
              function_scope.run(machine, step); 
            });
            return;
          default:
            this.scheduler.queue(function() { 
              function_scope.run(machine, machine.next()); 
            });
            return;
        }      
      }else if(Array.isArray(value) && value[0] === SLEEP){

        this.scheduler.sleep(function() { 
          function_scope.run(machine, machine.next()); 
        }, value[1]);

      }else if(Array.isArray(value) && value[0] === RECEIVE){

        let result = function_scope.receive(value[1]);

        if(result === NOMSG){
          this.scheduler.suspend(function() { 
            function_scope.run(machine, step); 
          });         
        }else{
          this.scheduler.queue(function() { 
            function_scope.run(machine, machine.next(result)); 
          });          
        }

      }else{
        this.scheduler.queue(function() { 
          function_scope.run(machine, machine.next()); 
        });        
      }
    }
  }
}

export default Process;