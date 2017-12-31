// Constructor
function Reference() {
    this.value = ++Reference.last;
}

// Static Methods
Reference.isReference = (reference) => reference instanceof Reference;
Reference.last = 0;

// Prototype Methods
Reference.prototype.value = function() {
    return this.value;
}

Reference.prototype.toString = function() {
    return "#Ref<"+this.value.toString()+">";
}

Reference.prototype.isUnbound = function() {
    return false;
}

Reference.prototype.match = function(X) {
    // Since 'false' is a legitimate atom in Erlang, we return undefined instead of false for a failure case
    if (Reference.isReference(X) && this.value === X.value) {
        return X;
    }
    else {
        return undefined;
    }
}


if (typeof exports != "undefined") {
    exports.Reference = Reference;
}
