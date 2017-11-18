var BigNumber = require("bignumber.js");


// Constructor
function ErlNumber(value) {
    this.value = new BigNumber(value);
}


// Static Methods
ErlNumber.isErlNumber = function(erlnum) {
    return erlnum instanceof ErlNumber;
}

ErlNumber.cloneNumber = function(erlnum) {
    return new ErlNumber(erlnum.toString());
}


// Prototype Methods
ErlNumber.prototype.toString = function() {
    return this.value.toString();
}

ErlNumber.prototype.isUnbound = function() {
    return false;
}

ErlNumber.prototype.match = function(value) {
    if (!this.value.equals(value)) {
        return undefined;
    }
    return value;
}


exports.ErlNumber = ErlNumber;
