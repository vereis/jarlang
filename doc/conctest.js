// Pid testing
function Pid(arg) {
    if (arguments.length === 3) {
        this.node = arguments[0];
        this.id_a = arguments[1];
        this.id_b = arguments[2];
    }
    else if (arg !== undefined && typeof (arg) === "function") {
        let res = arg();
        if (Array.isArray(res) && res.length === 3) {
            this.node = res[0];
            this.id_a = res[1];
            this.id_b = res[2];
        }
        else if (res.node && res.id_a && res.id_b) {
            this.node = res.node;
            this.id_a = res.id_a;
            this.id_b = res.id_b;
        }
        else {
            this.node = res.toString();
            this.id_a = res.toString();
            this.id_b = res.toString();
        }
    }
    else if (arg !== undefined && arg.toString !== undefined) {
        if (Array.isArray(arg) && arg.length === 3) {
            this.node = arg[0];
            this.id_a = arg[1];
            this.id_b = arg[2];
        }
        else if (arg.node && arg.id_a && arg.id_b) {
            this.node = arg.node;
            this.id_a = arg.id_a;
            this.id_b = arg.id_b;
        }
        else {
            this.node = arg.toString();
            this.id_a = arg.toString();
            this.id_b = arg.toString();
        }
    }
    else {
        this.node = 0;
        this.id_a = Pid.rng(0, 32767); // Max is 15 bits
        this.id_b = Pid.rng(0, 7);     // Max is 3 bits
    }
}

// Static Methods
Pid.isPid = (pid) => pid instanceof Pid;

Pid.rng = (min, max) => {
    min = Math.ceil(min);
    max = Math.floor(max);
    return Math.floor(Math.random() * (max - min)) + min;
}; 

// Prototype Methods
Pid.prototype.value = function() {
    return this.id_a;
}

Pid.prototype.toString = function() {
    return `<${this.node}.${this.id_a}.${this.id_b}>`;
}

Pid.prototype.isUnbound = function() {
    return false;
}

Pid.prototype.match = function(X) {
    // Since 'false' is a legitimate atom in Erlang, we return undefined instead of false for a failure case
    if (Pid.isPid(X) && this.value === X.value) {
        return X;
    }
    else {
        return undefined;
    }
}

// Return a shuffled copy of an array
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

// Set up a global processes array and a map of pids to array indices. 
// This allows us to have multiple processes simultaneously
const processes = [];
const pid_map   = {};

// setInterval with a time value of 0 indicates running the interval immediately (after any current work)
// is being done. Every time we have time to do some work, we map over the processes array and run any
// functions stored in said processes
setInterval(function() {
    shuffle(processes).map(process => {
        if (process.lambdas.length) {
            process.current_lambda = process.lambdas.shift();
            process.current_lambda();
            process.current_lambda = false;
        }
    });
}, 0);

// Constructor for process objects, automatically adds processes to pid_map and processes array.
function Process(lambda, pidGenerator) {
    this.pid = pidGenerator !== undefined ? new Pid(pidGenerator) : new Pid();
    this.lambdas  = [lambda];
    this.current_lambda = false;
    this.messages = [];
    this.stack    = {};

    pid_map[this.pid.toString()] = processes.length;
    processes.push(this);
}

// Static function to send messages to other processes by looking up pids in pid_map;
Process.sendMessage = (pid, msg) => {
    return Process.getProcess(pid).messages.push(msg);
};

// Static function to get a process from a pid
Process.getProcess = (pid) => {
    return processes[pid_map[pid.toString()]];
}

// Naive implementation go Erlang spawn method
function spawn(lambda, pidGenerator) {
    return new Process(lambda, pidGenerator).pid;
}

// Example usage: Ping-pong
let p1 = spawn(function() { 
    if (this.messages.length) { 
        let message = this.messages.shift();
        Process.sendMessage(message.pid, {pid: this.pid});

        console.log("PING: RECIEVED MSG FROM " + message.pid);
    } 
    else {
        console.log("PING: NO MESSAGE YET, WAITING...");
    }
    
    this.lambdas.push(this.current_lambda); 
}, [1, 2, 3]);

let p2 = spawn(function() { 
    if (this.messages.length) { 
        let message = this.messages.shift();        
        Process.sendMessage(message.pid, {pid: this.pid});

        console.log("PONG: RECIEVED MSG FROM " + message.pid);
    } 
    else {
        console.log("PONG: NO MESSAGE YET, WAITING...");
    }
    
    this.lambdas.push(this.current_lambda); 
}, {node: "a", id_a: "b", id_b: "c"});

Process.sendMessage(p1, {pid: p2});