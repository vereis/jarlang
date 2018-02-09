// Set up a global processes array and a map of pids to array indices. 
// This allows us to have multiple processes simultaneously
const processes = [];
const pid_map   = {};

// setInterval with a time value of 0 indicates running the interval immediately (after any current work)
// is being done. Every time we have time to do some work, we map over the processes array and run any
// functions stored in said processes
setInterval(function() {
    processes.map(process => {
        if (process.lambdas.length) {
            process.current_lambda = process.lambdas.shift();
            process.current_lambda();
            process.current_lambda = false;
        }
    });
}, 0);

// Constructor for process objects, automatically adds processes to pid_map and processes array.
function Process(lambda) {
    this.pid = getRandomInt(100000, 999999);
    this.lambdas  = [lambda];
    this.current_lambda = false;
    this.messages = [];
    this.stack    = {};

    function getRandomInt(min, max) {
        min = Math.ceil(min);
        max = Math.floor(max);
        return Math.floor(Math.random() * (max - min)) + min; //The maximum is exclusive and the minimum is inclusive
    }

    pid_map[this.pid] = processes.length;
    processes.push(this);
}

// Static function to send messages to other processes by looking up pids in pid_map;
Process.sendMessage = (pid, msg) => {
    return Process.getProcess(pid).messages.push(msg);
};

// Static function to get a process from a pid
Process.getProcess = (pid) => {
    return processes[pid_map[pid]];
}

// Naive implementation go Erlang spawn method
function spawn(lambda) {
    return new Process(lambda).pid;
}


// Example usage: Ping-pong
let p1 = spawn(function() { 
    if (this.messages.length) { 
        let message = this.messages.shift();
        Process.sendMessage(message.pid, {pid: this.pid});

        console.log("PING: RECIEVED MSG FROM <" + message.pid + ">");
    } 
    
    this.lambdas.push(this.current_lambda); 
});

let p2 = spawn(function() { 
    if (this.messages.length) { 
        let message = this.messages.shift();        
        Process.sendMessage(message.pid, {pid: this.pid});

        console.log("PONG: RECIEVED MSG FROM <" + message.pid + ">");
    } 
    
    this.lambdas.push(this.current_lambda); 
});

Process.sendMessage(p1, {pid: p2});