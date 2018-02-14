/**
 * Runtime environment for Jarlang
 * Provides all of the neccessary environment functions to allow Jarlang to work as intended
 * such as our process queue
 */
 const __erlang = (function(secondsPerTick) {
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

    const exports = {
        processes: [],
        pids: {},
        timer: setInterval(function() {
            shuffle(exports.processes).map(process => {
                if (process.lambdas.length) {
                    process.currentLambda = process.lambdas.shift();
                    process.currentLambda();
                    process.currentLambda = null;
                }
            });
        }, secondsPerTick !== undefined ? secondsPerTick : 1),
        spawn: function(lambda, pidGenerator) {
            return new Process(lambda, pidGenerator).pid;
        }
    };

    return exports;
 })();