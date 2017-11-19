// Constructor
function BitString() {
    this.value = undefined;
}

// Static Methods
BitString.isBitString = (bitstring) => bitstring instanceof BitString;

// Prototype Methods
BitString.prototype.value = function() {
    throw "BitString is not implemented yet";
}

BitString.prototype.toString = function() {
    throw "BitString is not implemented yet";
}

BitString.prototype.isUnbound = function() {
    return false;
}

BitString.prototype.match = function(X) {
    // Since 'false' is a legitimate atom in Erlang, we return undefined instead of false for a failure case
    if (BitString.isBitString(X) && this.value === X.value) {
        return X;
    }
    else {
        return undefined;
    }
}