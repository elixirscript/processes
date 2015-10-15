"use strict";
/* @flow */

let process_counter = -1;

class PID {
  constructor(){
    process_counter = process_counter + 1;
    this.id = process_counter;
  }

  toString(){
    return "PID#<0." + id + ".0>";
  }
}


export default PID;