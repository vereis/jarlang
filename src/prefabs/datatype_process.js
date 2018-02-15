function Process(lambda, pidGenerator) {
    this.pid = pidGenerator !== undefined ? new Pid(pidGenerator) : new Pid();
    this.lambdas  = [lambda];
    this.currentLambda = false;
    this.messages = [];
    this.stack    = {};

    this.registerProcess();
}

// Static function to send messages to other processes by looking up pids in pid_map;
Process.sendMessage = (pid, msg) => {
    return Process.getProcess(pid).messages.push(msg);
};

// Static function to get a process from a pid
Process.getProcess = (pid) => {
    return jrts.processes[jrts.pids[pid.toString()]];
};

// Spawns a new process with a given lambda and pid generator,
// automatically registering it with the runtime process queue
Process.spawn = (lambda, pidGenerator) => {
    return jrts.spawn(lambda, pidGenerator);
};

// Prototype Methods
Process.prototype.toString = function() {
    return this.pid.toString();
};

Process.prototype.registerProcess = function() {
    jrts.pids[this.pid.toString()] = jrts.processes.length;
    jrts.processes.push(this);
};

/* TEST:
var p1 = Process.spawn(function() {
    if (this.messages.length) {
        let message = this.messages.shift();
        Process.sendMessage(message.pid, {pid: this.pid});
        console.log("PING: RECEIEVED MSG FROM " + message.pid);
    }
    this.lambdas.push(this.currentLambda);
});
var p2 = Process.spawn(function() {
    if (this.messages.length) {
        let message = this.messages.shift();
        Process.sendMessage(message.pid, {pid: this.pid});
        console.log("PING: RECEIEVED MSG FROM " + message.pid);
    }
    this.lambdas.push(this.currentLambda);
});
Process.sendMessage(p1, {pid: p2});
*/