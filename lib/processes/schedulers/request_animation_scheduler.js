"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const default_scheduler_1 = __importDefault(require("./default_scheduler"));
/**
 * Scheduler for the process system.
 * Uses window.requestAnimationFrame to schedule process execution
 * Good for processes that do a lot of dom manipulation
 */
class RequestAnimationScheduler extends default_scheduler_1.default {
    constructor(throttle = 0, reductions_per_process = 8) {
        super(throttle, reductions_per_process);
    }
    _run(run) {
        window.requestAnimationFrame(run);
    }
}
exports.default = RequestAnimationScheduler;
