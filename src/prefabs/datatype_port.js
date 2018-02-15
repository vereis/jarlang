// Constructor
function Port() {
    this.value = undefined;
}

// Static Methods
Port.isPort = (port) => port instanceof Port;

// Prototype Methods
Port.prototype.value = function() {
    throw "Port is not implemented yet";
};

Port.prototype.toString = function() {
    throw "Port is not implemented yet";
};

Port.prototype.isUnbound = function() {
    return false;
};

Port.prototype.match = function(X) {
    // Since 'false' is a legitimate atom in Erlang, we return undefined instead of false for a failure case
    if (Port.isPort(X) && this.value === X.value) {
        return X;
    }
    else {
        return undefined;
    }
};


if (typeof exports != "undefined") {
    exports.Port = Port;
}
