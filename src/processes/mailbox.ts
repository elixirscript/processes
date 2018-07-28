class Mailbox {
  messages: any[]
  constructor() {
    this.messages = []
  }

  deliver(message: any): any {
    this.messages.push(message)
    return message
  }

  get(): any[] {
    return this.messages
  }

  isEmpty(): boolean {
    return this.messages.length === 0
  }

  removeAt(index: number): any {
    this.messages.splice(index, 1)
  }
}

export default Mailbox
