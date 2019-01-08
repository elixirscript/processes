/**
 * Manages a process's messages.
 * A message is anything sent to the process from another
 * process
 */
declare class Mailbox {
    private messages;
    constructor();
    deliver(message: any): any;
    get(): any[];
    isEmpty(): boolean;
    removeAt(index: number): any;
}
export default Mailbox;
