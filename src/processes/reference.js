"use strict";
/* @flow */

let ref_counter = -1;

class Reference {
  constructor(){
    ref_counter = ref_counter + 1;
    this.id = ref_counter;
    this.ref = Symbol();
  }

  toString(){
    return "Ref#<0.0.0." + this.id + ">";
  }
}


export default Reference;
