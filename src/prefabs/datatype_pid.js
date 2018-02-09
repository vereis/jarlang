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