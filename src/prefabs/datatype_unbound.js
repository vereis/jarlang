// Constructor 
function Unbound() {}

// Static Methods
Unbound.isUnbound = (X) => X instanceof Unbound;

// Prototype Methods
Unbound.prototype.toString = function() {
    throw `variable is unbound`;
}

Unbound.prototype.match = function(X) {
    return true;
}

Unbound.prototype.value = function() {
    return undefined;
}


if (typeof exports != "undefined") {
    exports.Unbound = Unbound;
}
