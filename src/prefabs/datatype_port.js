// Constructor
function Port() {
    this.value = undefined;
}

// Static Methods
Port.isPort = (port) => port instanceof Port;

// Prototype Methods
Port.prototype.value = function() {
    throw "Port is not implemented yet";
}

Port.prototype.toString = function() {
    throw "Port is not implemented yet";
}

Port.prototype.isUnbound = function() {
    return false;
}

Port.prototype.match = function(X) {
    if (Port.isPort(X) && this.value === X.value) {
        return true;
    }
    else {
        return false;
    }
}


if (typeof exports != "undefined") {
    exports.Port = Port;
}
