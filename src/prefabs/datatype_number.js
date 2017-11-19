var BigNumber = typeof BigNumber == "undefined" ? require("bignumber.js") : BigNumber;


// Constructor
function ErlNumber(val) {
    this.value = val instanceof BigNumber ? val : new BigNumber(val);
}


// Static Methods
ErlNumber.isErlNumber = function(val) {
    return val instanceof ErlNumber;
}

ErlNumber.cloneNumber = function(val) {
    return new ErlNumber(val.getValue());
}


// Prototype Methods
ErlNumber.prototype.getValue = function() {
    return this.value;
}

ErlNumber.prototype.toString = function() {
    return this.value.toString();
}

ErlNumber.prototype.typeOf = function() {
    return "ErlNumber";
}

ErlNumber.prototype.isInteger = function() {
    return this.value.isInteger();
}

ErlNumber.prototype.isFloat = function () {
    return !this.value.isInteger();
}

ErlNumber.prototype.isUnbound = function() {
    return false;
}

ErlNumber.prototype.add = function(val) {
    return new ErlNumber(this.value.plus(val instanceof ErlNumber ? val.getValue() : val));
}

ErlNumber.prototype.subtract = function(val) {
    return new ErlNumber(this.value.minus(val instanceof ErlNumber ? val.getValue() : val));
}

ErlNumber.prototype.multiply = function(val) {
    return new ErlNumber(this.value.times(val instanceof ErlNumber ? val.getValue() : val));
}

ErlNumber.prototype.divide = function(val) {
    return new ErlNumber(this.value.div(val instanceof ErlNumber ? val.getValue() : val));
}

ErlNumber.prototype.intDivide = function(val) {
    return new ErlNumber(this.value.divToInt(val instanceof ErlNumber ? val.getValue() : val));
}

ErlNumber.prototype.remainder = function(val) {
    return new ErlNumber(this.value.mod(val instanceof ErlNumber ? val.getValue() : val));
}

ErlNumber.prototype.equals = function(val) {
    return this.value.equals(val instanceof ErlNumber ? val.getValue() : val);
}

ErlNumber.prototype.lessThan = function(val) {
    return this.value.lessThan(val instanceof ErlNumber ? val.getValue() : val);
}

ErlNumber.prototype.lessThanOrEq = function(val) {
    return this.value.lessThanOrEqualTo(val instanceof ErlNumber ? val.getValue() : val);
}

ErlNumber.prototype.greaterThan = function(val) {
    return this.value.greaterThan(val instanceof ErlNumber ? val.getValue() : val);
}

ErlNumber.prototype.greaterThanOrEq = function(val) {
    return this.value.greaterThanOrEqualTo(val instanceof ErlNumber ? val.getValue() : val);
}

ErlNumber.prototype.match = function(val) {
    if (isNaN(val) || !this.equals(val)) {
        return undefined;
    }
    return val;
}

//This is to prevent errors if the code is loaded in a browser
if(typeof exports != "undefined"){
    exports.ErlNumber = ErlNumber;
}
