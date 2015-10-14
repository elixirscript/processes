function start(module, args){
  let pid = start_process(module, args);
  return [Symbol.for("ok"), pid];
}

function start_link(module, args){
  let pid = start_process(module, args);
  return [Symbol.for("ok"), pid];
}

function start_process(module, args){
  return self.scheduler.spawn(function*(){
    let [ok, state] = module.init.apply(null, [args]);
    yield self.scheduler.current_process.dictionary["state"] = state;

    while(true){
      yield self.scheduler.receive(function(args){
        if(args[0] === "call"){
          let result = module.handle_call(args[1], args[2], self.scheduler.current_process.dictionary["state"]);
          self.scheduler.current_process.dictionary["state"] = result[2];

          self.scheduler.send(args[2], result[1]);

        }else if(args[0] === "cast"){
          let result = module.handle_cast(args[1], self.scheduler.current_process.dictionary["state"]);
          self.scheduler.current_process.dictionary["state"] = result[1];

          self.scheduler.send(args[2], Symbol.for("ok"));

        }        
      });
    }
  });
}

function* call(server, request){
  self.scheduler.send(server, ["call", request, self.scheduler.pid()]);

  return yield self.scheduler.receive(function(args){
    return args;
  });
}

function* cast(server, request){
  self.scheduler.send(server, ["cast", request, self.scheduler.pid()]);

  return yield self.scheduler.receive(function(args){
    return args;
  });  
}

export default { start, start_link, call, cast };