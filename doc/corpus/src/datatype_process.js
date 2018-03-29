/**
 * Class constructor for representing Erlang Processes in Jarlang's runtime
 */

const Process = (() => {
    // Private registerProcess token
    const registerProcess = Symbol("registerProcess");

    return class Process extends ErlangDatatype {
        constructor(lambda, pidGenerator) {
            super();
            this.pid = pidGenerator !== undefined ? new Pid(pidGenerator) : new Pid();
            this.lambdas  = [lambda];
            this.currentLambda = false;
            this.messages = [];
            this.stack    = {};
            this.value    = null;
            this.lastRun  = Date.now();
        
            this[registerProcess]();
        }

        toString() {
            return this.pid.toString();
        }

        sendMessage(msg) {
            return this.messages.push(msg);
        }

        getValue() {
            return this.pid;
        }

        matchMessages(lambda) {
            const unmatchedMessages = [];
            const matchedMessages = this.messages.filter((message) => {
                let predicatePasses = lambda(message);
                if (predicatePasses) {
                    return true;
                }
                else {
                    unmatchedMessages.push(message);
                }
            });
            this.messages = unmatchedMessages;
            return matchedMessages;
        }

        setBehaviour(fn) {
            this.lambdas.push(fn);
        }

        restartBehaviour() {
            this.lambdas.push(this.currentLambda);
        }

        [registerProcess]() {
            jrts.pids[this.pid.toString()] = jrts.processes.length;
            jrts.processes.push(this);
        }

        static isProcess(process) {
            return process instanceof Process;
        }

        static sendMessage(pid, msg) {
            return Process.getProcess(pid).sendMessage(msg);
        }

        static getProcess(v) {
            return jrts.processes[(Atom.isAtom(v) ? jrts.pids[jrts.atoms[v.getValue()].toString()] : jrts.pids[v.toString()])];
        }

        static spawn(lambda, pidGenerator) {
            return jrts.spawn(lambda, pidGenerator);
        }
    };
})();