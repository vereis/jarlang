/**
 * Runtime environment for Jarlang
 * Provides all of the neccessary environment functions to allow Jarlang to work as intended
 * such as our process queue
 */
const jrts = (function (secondsPerTick) {
    'use strict';

    // Return a shuffled copy of an array, this is used so that the processes queue
    // is mapped over randomly each tick
    function shuffle(input) {
        let array = [...input];

        let count = array.length;
        let randomnumber;
        let temp;
        while (count) {
            randomnumber = Math.random() * count-- | 0;
            temp = array[count];
            array[count] = array[randomnumber];
            array[randomnumber] = temp;
        }

        return array;
    }

    // Spawn a new process which registers itself with the processes queue
    function spawnProcess(lambda, pidGenerator) {
        return new Process(lambda, pidGenerator).pid;
    }

    function jsToErlang(variable) {
        let erlVar = null;
        switch (typeof variable) {
            case "number":
                erlVar = new ErlNumber(variable);
                break;
            case "string":
                erlVar = new List(...(variable.split("").map(char => char.charCodeAt(0))));
                break;
            case "object":
                if (Array.isArray(variable)) {
                    erlVar = new List(...variable);
                } else if (variable.constructor.name === "Uint8Array") {
                    erlVar = new BitString(...variable);
                } else {
                    erlVar = new ErlMap(variable);
                }
                break;
            default: erlVar = variable;
        }

        return erlVar;
    }

    function erlangToJs(variable) {
        let jsVar = null;
        switch (variable.constructor.name) {
            case "Atom":
                jsVar = variable.value;
                break;
            case "BitString":
                jsVar = variable.value;
                break;
            case "Fun":
                jsVar = variable.value;
                break;
            case "List":
                jsVar = List.isString(variable) ? 
                    [...variable].map(charCode => String.fromCharCode(charCode)).join("") : 
                    [...variable];
                break;
            case "ErlMap":
                jsVar = variable.value;
                break;
            case "ErlNumber":
                jsVar = Number(variable.getValue().valueOf());
                break;
            case "Pid":
                jsVar = variable.value;
                break;
            case "Port":
                jsVar = variable.value;
                break;
            case "Process":
                jsVar = variable;
                break;
            case "Reference":
                jsVar = variable.value;
                break;
            case "Tuple":
                jsVar = [...variable];
                break;
            case "Unbound":
                jsVar = undefined;
                break;
            default: jsVar = variable;
        }

        return jsVar;
    }

    // Set up timer to iterate through processes
    const eventLoop = setInterval(function () {
        shuffle(exports.processes).map(process => {
            if (process.lambdas.length) {
                process.currentLambda = process.lambdas.shift();
                process.value = process.currentLambda();
                process.currentLambda = null;
                process.lastRun = Date.now();
            }
        });
    }, secondsPerTick !== undefined ? secondsPerTick : 1);

    // const garbageCollection = setInterval(function() {
    //     for (let pid in jrts.pids) {
    //         if (Date.now() - Process.getProcess(pid).lastRun > 1000/* * 60 * 2*/) {
    //             delete jrts.processes[jrts.pids[pid]];
    //             delete jrts.pids[pid];
    //             console.log("deleted process: ", pid);
    //         }
    //     }
    // }, 5000);

    const exports = {
        processes: [],
        pids: {},
        atoms: {},
        timer: eventLoop,
        spawn: spawnProcess,
        jsToErlang: jsToErlang,
        erlangToJs: erlangToJs,
    };

    return exports;
})();