"use strict";

class ProcessQueue {
  constructor(pid){
    this.pid = pid;
    this.tasks = [];
  }

  empty(){
    return this.tasks.length === 0;
  }

  add(task){
    this.tasks.push(task);
  }

  next(){
    return this.tasks.shift();
  }
}

class Scheduler {
    constructor(throttle = 0, reductions_per_process = 8){
        this.isRunning = false;
        this.invokeLater = function (callback) { setTimeout(callback, throttle); };

        // In our case a reduction is equal to a task call
        // Controls how many tasks are called at a time per process
        this.reductions_per_process = reductions_per_process;
        this.queues = new Map();
        this.run();
  }

  addToQueue(pid, task){
    if(!this.queues.has(pid)){
      this.queues.set(pid, new ProcessQueue(pid));
    }

    this.queues.get(pid).add(task);
  }

  removePid(pid){
    this.isRunning = true;

    this.queues.delete(pid);

    this.isRunning = false;
  }

    run(){
        let iter = this.queues.entries();
        let next = iter.next();
        this.invokeLater(this.do_run(next, iter, this.reductions_per_process));
    }

    do_run(entry, queueIterator, reductions){
        if(entry.done == true){
            let iter = this.queues.entries();
            let next = iter.next();
            this.invokeLater(() => this.do_run(next, iter, this.reductions_per_process));
        }else if(this.isRunning){
            this.invokeLater(this.do_run(entry, queueIterator, reductions));
        }else if(reductions == 0 || !entry.value[1] || entry.value[1].empty()){
            let next = queueIterator.next();
            this.invokeLater(this.do_run(next, queueIterator, this.reductions_per_process));
        }else{
            let queue = entry.value[1];
            let task = queue.next();
            this.isRunning = true;

            let result;

            try{
                result = task();
            }catch(e){
                console.error(e);
                result = e;
            }

            this.isRunning = false;

            if (result instanceof Error) {
                throw result;
            }

            this.invokeLater(this.do_run(entry, queueIterator, reductions - 1));
        }
    }

  addToScheduler(pid, task, dueTime = 0) {
    if(dueTime === 0){
      this.invokeLater(() => {
        this.addToQueue(pid, task);
      });
    }else{
      setTimeout(() => {
        this.addToQueue(pid, task);
      }, dueTime);
    }
  };

  schedule(pid, task){
    this.addToScheduler(pid, () => { task(); });
  }

  scheduleFuture(pid, dueTime, task){
    this.addToScheduler(pid, () => { task(); }, dueTime);
  }
}

export default Scheduler;
