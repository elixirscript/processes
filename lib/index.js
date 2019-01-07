"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const process_system_1 = __importDefault(require("./processes/process_system"));
const request_animation_scheduler_1 = __importDefault(require("./processes/schedulers/request_animation_scheduler"));
const default_scheduler_1 = __importDefault(require("./processes/schedulers/default_scheduler"));
exports.default = {
    ProcessSystem: process_system_1.default,
    RequestAnimationScheduler: request_animation_scheduler_1.default,
    DefaultScheduler: default_scheduler_1.default,
};
