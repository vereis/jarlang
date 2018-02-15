var BigNumber = typeof BigNumber == "undefined" && typeof require == "function" ? require("bignumber.js") : BigNumber;

// Constructor
function Float(val) {
    this.value = val instanceof BigNumber ? val : new BigNumber(val);
}

// Static Methods
Float.isFloat = function(val) {
    return val instanceof Float;
};

Float.cloneNumber = function(val) {
    return new Float(val.getValue());
};

// Prototype Methods
Float.prototype.getValue = function() {
    return this.value;
};

Float.prototype.toString = function() {
    return this.value.toString();
};

Float.prototype.typeOf = function() {
    return "Float";
};

Float.prototype.isUnbound = function() {
    return false;
};

Float.prototype.add = function(val) {
    return new Float(this.value.plus(val instanceof Float ? val.getValue() : val));
};

Float.prototype.subtract = function(val) {
    return new Float(this.value.minus(val instanceof Float ? val.getValue() : val));
};

Float.prototype.multiply = function(val) {
    return new Float(this.value.times(val instanceof Float ? val.getValue() : val));
};

Float.prototype.divide = function(val) {
    return new Float(this.value.div(val instanceof Float ? val.getValue() : val));
};

Float.prototype.intDivide = function(val) {
    return new Float(this.value.divToInt(val instanceof Float ? val.getValue() : val));
};

Float.prototype.remainder = function(val) {
    return new Float(this.value.mod(val instanceof Float ? val.getValue() : val));
};

Float.prototype.equals = function(val) {
    return this.value.equals(val instanceof Float ? val.getValue() : val);
};

Float.prototype.lessThan = function(val) {
    return this.value.lessThan(val instanceof Float ? val.getValue() : val);
};

Float.prototype.lessThanOrEq = function(val) {
    return this.value.lessThanOrEqualTo(val instanceof Float ? val.getValue() : val);
};

Float.prototype.greaterThan = function(val) {
    return this.value.greaterThan(val instanceof Float ? val.getValue() : val);
};

Float.prototype.greaterThanOrEq = function(val) {
    return this.value.greaterThanOrEqualTo(val instanceof Float ? val.getValue() : val);
};

Float.prototype.match = function(val) {
    if (isNaN(val) || !this.equals(val)) {
        return undefined;
    }
    return val;
};


if (typeof exports != "undefined") {
    exports.Float = Float;
}
