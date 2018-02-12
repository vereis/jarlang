// Constructor
function BitString () {
    var tmp = [];

    for (b in arguments) {
        if (!(ErlNumber.isErlNumber(b) && b.isInteger()) && !List.isString(b)) {
            throw "bad argument";
        }

        if (List.isString(b)) {
            for (c in b.split("")) {
                tmp.push(c.charCodeAt(0));
            }
        }
        else {
            tmp.push(b);
        }
    }

    this.value = Uint8Array.of(...tmp);
}

// Static Methods
BitString.isBitString = (bitstring) => bitstring instanceof BitString;
BitString.isBinary = (bitstring) => bitstring instanceof BitString; // todo

// Prototype Methods
BitString.prototype.value = function() {
    return this.value;
};

BitString.prototype.toString = function() {
    return `<<${this.value.join(",")}>>`;
};

BitString.prototype.isUnbound = function() {
    return false;
};

BitString.prototype.match = function(X) {
    // Since 'false' is a legitimate atom in Erlang, we return undefined instead of false for a failure case
    if (BitString.isBitString(X) && this.value === X.value) {
        return X;
    }
    else {
        return undefined;
    }
};


if (typeof exports != "undefined") {
    exports.BitString = BitString;
}
