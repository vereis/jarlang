var pingprocess = Process.spawn(function() {
    let shouldRestartBehaviour = true;
    let matchedMessages = this.matchMessages(message => message.pid !== undefined);
    if (matchedMessages) {
        matchedMessages.map(message => {
            if (message.stopProcess !== undefined) {
                console.log("Ping - stop command receieved. Stopping process");
                shouldRestartBehaviour = false;
            }
            else {
                console.log("Ping - message receieved from:", message.pid.toString());
                Process.sendMessage(message.pid, {pid: this.pid});
            }
        });
    }

    shouldRestartBehaviour ? this.restartBehaviour() : this.setBehaviour(()=>{});
});

var pongprocess = Process.spawn(function() {
    let shouldRestartBehaviour = true;
    let matchedMessages = this.matchMessages(message => message.pid !== undefined);
    if (matchedMessages) {
        matchedMessages.map(message => {
            if (message.stopProcess !== undefined) {
                console.log("Pong - stop command receieved. Stopping process");
                shouldRestartBehaviour = false;
            }
            else {
                console.log("Pong - message receieved from:", message.pid.toString());
                Process.sendMessage(message.pid, {pid: this.pid});
            }
        });
    }

    shouldRestartBehaviour ? this.restartBehaviour() : this.setBehaviour(()=>{});
});

// Start pingpong chain
pingprocess.sendMessage({pid: pongprocess});

// After 15s, stop ping pong chain
setTimeout(() => {
    pongprocess.sendMessage({pid: 1, stopProcess: true});
}, 15 * 1000);