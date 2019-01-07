declare class Mailbox {
    messages: any[];
    constructor();
    deliver(message: any): any;
    get(): any[];
    isEmpty(): boolean;
    removeAt(index: number): any;
}
export default Mailbox;
