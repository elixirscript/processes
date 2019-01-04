import test from 'ava'
import Process from '../src/processes/process'
import Mailbox from '../src/processes/mailbox'
import {PID} from 'erlang-types'

test('constructor', function(t) {
  const pid = new PID()
  const mailbox = new Mailbox()
  const process = new Process()

  t.is(mailbox.messages.length, 0)
})
