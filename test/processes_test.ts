import test from 'ava';
import Processes from '../src/index.js';

let system = null;

test.beforeEach(t => {
  system = new Processes.ProcessSystem();
});

test(function* testSpawn(t) {
  const pid = system.spawn(function*() {
    yield 1;
  });

  t.is(system.list().length, 2);
  t.is(system.list()[1], pid);
});

test(function* testSpawnLink(t) {
  const pid = system.spawn_link(function*() {
    yield 1;
  });

  t.is(system.list().length, 2);
  t.true(system.links.get(pid).has(system.list()[0]));
  t.true(system.links.get(system.list()[0]).has(pid));
});
