// Constructor
function Fun() {
    this.value = undefined;
}

// Static Methods
Fun.isFun = (fun) => fun instanceof Fun;

// Prototype Methods
Fun.prototype.value = function() {
    throw "Fun is not implemented yet";
}

Fun.prototype.toString = function() {
    throw "Fun is not implemented yet";
}

Fun.prototype.isUnbound = function() {
    return false;
}

Fun.prototype.match = function(X) {
    if (Fun.isFun(X) && this.value === X.value) {
        return true;
    }
    else {
        return false;
    }
}


if (typeof exports != "undefined") {
    exports.Fun = Fun;
}
