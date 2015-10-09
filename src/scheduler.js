/* @flow */
"use strict";

import Mailbox from "./mailbox";
import Process from "./process";
import Rx from "rx";

export const NORMAL = Symbol();
export const SUSPEND = Symbol();
export const CONTINUE = Symbol();
export const RECEIVE = Symbol();
export const SEND = Symbol();

export const SLEEPING = Symbol();
export const RUNNING = Symbol();
export const SUSPENDED = Symbol();
export const STOPPED = Symbol();

export const SLEEP = Symbol();
export const EXIT = Symbol();

class Scheduler{

  constructor(){
    this.process_counter = 0;
    this.pids = new Map();
    this.mailboxes = new Map();
    this.names = new Map();
    this.links = new Map();

    this.current_process = null;
    this.rxScheduler = Rx.Scheduler.default;
    this.suspended = new Map();
  }

  set_current(process){
    this.current_process = process;
    this.current_process.status = RUNNING;
  }

  spawn(fun, ...args){
    return this.add_proc(fun, args, false).pid;
  }

  spawn_link(fun, ...args){
    return this.add_proc(fun, args, true).pid; 
  }

  add_proc(fun, args, linked){
    this.process_counter = this.process_counter + 1;
    let newpid = this.process_counter;
    let mailbox = new Mailbox();
    let newproc = new Process(newpid, fun, args, mailbox, this);

    this.pids.set(newpid, newproc);
    this.mailboxes.set(newpid, mailbox);

    if(linked){
      this.links.get(this.current_process.pid).push(pid);
      this.links.set(newpid, [this.current_process.pid]);
    }else{
      this.links.set(newpid, []);
    }

    newproc.start();
    return newproc;
  }

  remove_proc(pid, exitreason){
    this.pids.delete(pid);
    this.unregister(pid);

    for (let linkpid in this.links.get(pid)) {
       linkpid = Number(linkpid);

       if (exitreason != Normal) {
          this.pids.get(linkpid).deliver({ Signal: EXIT, From: pid, Reason: exitreason });
       }
    }

    this.links.delete(pid);
  }

  register(name, pid){
    if(!this.names.has(name)){
      this.names.set(name, pid)
    }else{
      throw new Error("Name is already registered to another process");
    }
  }

  registered(name){
    return this.names.has(name) ? this.names.get(name) : null;
  }

  unregister(pid){
    for(let name of this.names.keys()){
      if(this.names.has(name) && this.names.get(name) === pid){
        this.names.delete(name);
      }
    }
  }

  pid(){
    return this.current_process.pid;
  }

  pidof(id){
    if (typeof(id) === "number") {
       return this.pids.has(id) ? id : null;
    } else if (id instanceof Process) {
       return id.pid;
    } else {
       let pid = this.registered(id);
       if (pid === null)
          throw("Er: Process name not registered: " + 
                id + " (" + typeof(id) + ")");
       return pid;
    }
  }

  send(id, msg) {
    const pid = this.pidof(id);

    if(pid){
      this.mailboxes.get(pid).deliver(msg);

      if(this.suspended.has(pid)){
        let fun = this.suspended.get(pid);
        this.suspended.delete(pid);
        this.queue(fun);
      }
    }

    return msg;
  }

  suspend(fun){
    this.current_process.status = SUSPENDED;
    this.suspended.set(this.current_process.pid, fun);
  }

  sleep(fun, time){
    this.current_process.status = SLEEPING;
    Rx.Scheduler.default.scheduleFuture(0, time, fun);
  }

  queue(fun){
    this.rxScheduler.schedule(0, fun);  
  }

  exit(...args){
    switch(args.length) {
    case 2:
       if (args[1] != NORMAL) {
          this.mailboxes.get(args[0]).deliver({ Signal: EXIT, From: this.pid(), Reason: args[1] });
       }else{
          this.remove_proc(args[0], args[1]);       
       }
       break;
    case 1:
       this.current_process.exit(args[0]);
       break;
    case 0:
       this.current_process.exit(Normal);
       break;
    }
  }
}

export default Scheduler;