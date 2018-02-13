// Keep track of declared atoms
// todo: Expand to include reserved atoms
const atomTable = {};

// Constructor
function Atom(name) {
    this.value = name;

    if (!atomTable[name]) {
        atomTable[name] = this;
    }
}

// Static Methods
Atom.isAtom = (atom) => atom instanceof Atom;
Atom.cloneAtom = (atom) => atom;
Atom.exists = (name) => !!atomTable[name];

// Prototype Methods
Atom.prototype.getValue = function() {
    return this.value;
}

Atom.prototype.toString = function() {
    return this.value.toString();
}

Atom.prototype.isUnbound = function() {
    return false;
}

Atom.prototype.match = function(X) {
    if (Atom.isAtom(X) ? this.value === X.value : this.value === X) {
        return true;
    }
    else {
        return false;
    }
}


if (typeof exports != "undefined") {
    exports.Atom = Atom;
}
