function start(module, args){
  return [Symbol.for("ok"), self.scheduler.spawn(start_process(module, args))];
}

function start_link(module, args){
  return [Symbol.for("ok"), self.scheduler.spawn_link(start_process(module, args))];
}

function start_process(module, args){
  return function*(){
    let [ok, state] = module.init.apply(null, [args]);
    yield self.scheduler.put("state", state);

    try{
      while(true){
        yield self.scheduler.receive(function(args){
          let command = args[0];

          switch(command){
            case "call":
              var request = args[1];
              var sender = args[2];

              var [reply, response, new_state] = module.handle_call(request, sender, self.scheduler.get("state"));
              self.scheduler.put("state", new_state);

              self.scheduler.send(sender, response);
              break;

            case "cast":
              var request = args[1];
              var sender = args[2];

              var [reply, new_state] = module.handle_cast(request, self.scheduler.get("state"));

              self.scheduler.put("state", new_state);
              self.scheduler.send(args[2], Symbol.for("ok"));

              break;

            case "stop":
              throw "stop";
          }       
        });
      }
    }catch(e){
      if(e !== "stop"){
        throw e;
      }
    }
  }
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

function stop(server, request){
  self.scheduler.send(server, ["stop"]); 
}

export default { start, start_link, call, cast, stop };