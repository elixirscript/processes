"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
/**
 * Manages a process's messages.
 * A message is anything sent to the process from another
 * process
 */
class Mailbox {
    constructor() {
        this.messages = [];
    }
    deliver(message) {
        this.messages.push(message);
        return message;
    }
    get() {
        return this.messages;
    }
    isEmpty() {
        return this.messages.length === 0;
    }
    removeAt(index) {
        this.messages.splice(index, 1);
    }
}
exports.default = Mailbox;
