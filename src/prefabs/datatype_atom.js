// Constructor
function Atom(name) {
    this.value = name;

    if (!jrts.atoms[name]) {
        jrts.atoms[name] = this;
    }
}

// Static Methods
Atom.isAtom = (atom) => atom instanceof Atom;
Atom.cloneAtom = (atom) => atom;
Atom.exists = (name) => !!jrts.atoms[name];

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
    // Since 'false' is a legitimate atom in Erlang, we return undefined instead of false for a failure case
    if (Atom.isAtom(X) ? this.value === X.value : this.value === X) {
        return X;
    }
    else {
        return undefined;
    }
}


if (typeof exports != "undefined") {
    exports.Atom = Atom;
}
