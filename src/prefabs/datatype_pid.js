// Constructor
function Pid() {
    this.value = undefined;
}

// Static Methods
Pid.isPid = (pid) => pid instanceof Pid;

// Prototype Methods
Pid.prototype.value = function() {
    throw "Pid is not implemented yet";
}

Pid.prototype.toString = function() {
    throw "Pid is not implemented yet";
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