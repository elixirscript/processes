import test from 'ava'
import Processes from '../src/index'

let system = null

test.beforeEach(t => {
  system = new Processes.ProcessSystem()
})

test('spawn', function testSpawn(t) {
  const pid = system.spawn(function*() {
    yield 1
  })

  t.is(system.list().length, 2)
  t.is(system.list()[1], pid)
})

test('spawn_link', function testSpawnLink(t) {
  const pid = system.spawn_link(function*() {
    yield 1
  })

  t.is(system.list().length, 3)
  t.true(system.links.get(pid).has(system.list()[0]))
  t.true(system.links.get(system.list()[0]).has(pid))
})
