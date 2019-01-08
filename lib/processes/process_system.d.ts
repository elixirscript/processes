import Mailbox from './mailbox';
import Process from './process';
import DefaultScheduler from './schedulers/default_scheduler';
import { PID, Reference } from 'erlang-types';
/**
 * Manages all of the processes.
 */
declare class ProcessSystem {
    pids: Map<PID, Process>;
    mailboxes: Map<PID, Mailbox>;
    names: Map<any, PID>;
    links: Map<PID, Set<PID>>;
    monitors: Map<Reference, {
        monitor: PID;
        monitee: PID;
    }>;
    current_process: Process | null;
    scheduler: DefaultScheduler;
    suspended: Map<PID, () => any>;
    main_process_pid: PID;
    constructor(scheduler?: DefaultScheduler);
    static run(fun: Function, args: any[], context?: any): IterableIterator<any>;
    /**
     * Starts a process represented by the given generator function
     * @param args Either a generator function or a module, function and arguments
     */
    spawn(...args: any[]): PID;
    /**
     * Starts a process using the generator function from the specified module
     * @param args Either a generator function or a module, function and arguments
     */
    spawn_link(...args: any[]): PID;
    /**
     * links the current process with the process from the given pid
     * @param pid pid of the process to link to
     */
    link(pid: PID): void;
    /**
     * unlinks the current process from the process from the given pid
     * @param pid pid of the process to link to
     */
    unlink(pid: PID): void;
    /**
     * Spawns a process and then monitors it
     * @param args Either a generator function or a module, function and arguments
     */
    spawn_monitor(...args: any[]): [PID, Reference];
    /**
     * Monitors the given process
     * @param pid pid of the process to link to
     */
    monitor(pid: PID): Reference | null;
    /**
     * Removes the monitor
     * @param ref Reference to monitor
     */
    demonitor(ref: Reference): Boolean;
    /**
     * Sets the current process
     * @param id PID or name of process
     */
    set_current(id: PID | any): void;
    add_proc(fun: GeneratorFunction, args: any[], linked: boolean, monitored: boolean): Process;
    remove_proc(pid: PID, exitreason: any): void;
    /**
     * registers the given name to the pid
     * @param name The name to give the process
     * @param pid The pid of the process
     */
    register(name: any, pid: PID): void;
    /**
     * Finds a process by the given name
     * @param name the name of the process
     */
    whereis(name: any): any | null;
    /**
     * returns the liast of names that are registered
     */
    registered(): IterableIterator<any>;
    /**
     * unregisters the names associated with the pid
     * @param pid The pid of the process
     */
    unregister(pid: PID): void;
    /**
     * Returns the PID of the current process
     */
    pid(): PID | null;
    /**
     * takes the input and tries to find the pid. Input can be a `pid`, `Process`, or name the pid is associated with
     * @param id The registered name or pid of the process
     */
    pidof(id: PID | any): PID | null;
    /**
     * sends a message the the process represented by the pid
     * @param id
     * @param msg
     */
    send(id: any, msg: any): any;
    /**
     * Tells the current process to receive a message that the function can handle.
     * If no match then the process is put in the suspended state until a message arrives
     * or the timeout is reached.
     * If the timeout is reached and no msg matches, then the timeoutFn is called
     * @param fun
     * @param timeout
     * @param timeoutFn
     */
    receive(fun: Function, timeout?: number, timeoutFn?: () => boolean): (number | symbol | Function | null)[];
    /**
     * puts the current process to sleep
     * @param duration
     */
    sleep(duration: number | symbol): [symbol, number | symbol];
    /**
     * Suspends the current process
     * @param fun
     */
    suspend(fun: () => any): void;
    /**
     * Makes current process go to sleep
     * @param fun
     * @param time
     */
    delay(fun: () => any, time: number): void;
    /**
     * Schedules execution of a process reduction
     * @param fun
     * @param pid
     */
    schedule(fun: () => any, pid?: PID): void;
    /**
     * terminates the current process with the given reason.
     * @param one
     * @param two
     */
    exit(one: PID | any, two?: any): void;
    /**
     * terminates the current process with an error
     * @param reason
     */
    error(reason: any): void;
    /**
     * Sets flags on the current process.
      - Note: the only flag respected is the `Symbol.for("trap_exit")` flag.
      If value is `true`, then exit signals from linked processes are turned into
      messages and sent to the current processes mailbox.
      If value is `false`, the exit is treated as normal and terminates the process.
      Setting it to `true` is useful for supervising processes.
     * @param args
     */
    process_flag(...args: any[]): any;
    /**
     * Adds a value to the current process's dictionary
     * @param key
     * @param value
     */
    put(key: string, value: any): void;
    /**
     * Gets the current process's dictionary
     */
    get_process_dict(): object;
    /**
     * Gets a value from the current process's dictionary or the default if key not in dictionary
     * @param key
     * @param default_value
     */
    get(key: string, default_value?: any): any;
    /**
     * Gets all the keys from the current process's dictionary
     * @param value
     */
    get_keys(value: any): string[];
    /**
     * Removes the key and the associated value from the current process's dictionary
     *
     * If no key is given, removes all entries from the current process's dictionary
     * @param key the key to remove
     */
    erase(key?: string): void;
    /**
     * Returns if the given pid is alive
     * @param pid
     */
    is_alive(pid: any): boolean;
    /**
     * Returns a list of all the pids
     */
    list(): PID[];
    /**
     * Returns a unique reference
     */
    make_ref(): Reference;
    private readonly currentProcess;
}
export default ProcessSystem;
