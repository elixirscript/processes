import test from 'ava'
import ProcessQueue from '../src/processes/process_queue'
import {PID} from 'erlang-types'

test('constructor', function(t) {
  const pid = new PID()
  const queue = new ProcessQueue(pid)

  t.is(queue.pid.id, pid.id)
  t.is(queue.tasks.length, 0)
  t.true(queue.empty())
})

test('add', function(t) {
  const pid = new PID()
  const queue = new ProcessQueue(pid)

  queue.add(() => 'Hi')
  t.false(queue.empty())
})

test('next', function(t) {
  const pid = new PID()
  const queue = new ProcessQueue(pid)
  const func = () => 'Hi'

  queue.add(func)

  t.is(queue.next(), func)
})
