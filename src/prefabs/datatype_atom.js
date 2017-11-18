// Constructor
function Atom(name) {
    this.value = name;
}

// Static Methods
Atom.isAtom = (atom) => atom instanceof Atom;
Atom.cloneAtom = (atom) => atom;

// Prototype Methods
Atom.prototype.value = function() {
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