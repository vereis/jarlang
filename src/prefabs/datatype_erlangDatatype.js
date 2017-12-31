// Constructor
function ErlangDatatype() {
    this.value = null;
}

// Static Methods
ErlangDatatype.isErlangDatatype = function(X) {
    return X instanceof ErlangDatatype;
}

// Prototype Methods
ErlangDatatype.prototype.value = function() {
    return this.value;
}

ErlangDatatype.prototype.toString = function() {
    return this.value.toString();
}

ErlangDatatype.prototype.match = function(X) {
    if (ErlangDatatype.isErlangDatatype(X) ? this.value === X.value : this.value === X) {
        return X;
    }
    else {
        return undefined;
    }
}


if (typeof exports != "undefined") {
    exports.ErlangDatatype = ErlangDatatype;
}
