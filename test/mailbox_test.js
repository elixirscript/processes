import test from 'ava'
import Mailbox from '../src/processes/mailbox'

test('constructor', function(t) {
  const mailbox = new Mailbox()

  t.is(mailbox.messages.length, 0)
})

test('get', function(t) {
  const mailbox = new Mailbox()

  t.is(mailbox.get().length, 0)

  mailbox.deliver('Hi')

  t.is(mailbox.get().length, 1)
})

test('isEmpty', function(t) {
  const mailbox = new Mailbox()

  t.true(mailbox.isEmpty())

  mailbox.deliver('Hi')

  t.false(mailbox.isEmpty())
})

test('removeAt', function(t) {
  const mailbox = new Mailbox()

  mailbox.deliver('Hi')
  mailbox.deliver('Goodbye')

  mailbox.removeAt(0)

  t.is(mailbox.get()[0], 'Goodbye')
})
