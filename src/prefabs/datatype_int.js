var BigNumber = typeof BigNumber == "undefined" && typeof require == "function" ? require("bignumber.js") : BigNumber;

// Constructor
function Int(val) {
    this.value = val instanceof BigNumber ? val : new BigNumber(val);
}

// Static Methods
Int.isInt = function(val) {
    return val instanceof Int;
};

Int.cloneNumber = function(val) {
    return new Int(val.getValue());
};

// Prototype Methods
Int.prototype.getValue = function() {
    return this.value;
};

Int.prototype.toString = function() {
    return this.value.toString();
};

Int.prototype.typeOf = function() {
    return "Int";
};

Int.prototype.isUnbound = function() {
    return false;
};

Int.prototype.add = function(val) {
    if (val instanceof Float) {
        return new Float(this.value.plus(val.getValue());
    }
    return new Int(this.value.plus(val instanceof Int ? val.getValue() : val));
};

Int.prototype.subtract = function(val) {
    if (val instanceof Float) {
        return new Float(this.value.minus(val.getValue());
    }
    return new Int(this.value.minus(val instanceof Int ? val.getValue() : val));
};

Int.prototype.multiply = function(val) {
    if (val instanceof Float) {
        return new Float(this.value.times(val.getValue());
    }
    return new Int(this.value.times(val instanceof Int ? val.getValue() : val));
};

Int.prototype.divide = function(val) {
    if (val instanceof Float) {
        return new Float(this.value.div(val.getValue());
    }
    return new Int(this.value.div(val instanceof Int ? val.getValue() : val));
};

Int.prototype.intDivide = function(val) {
    return new Int(this.value.divToInt(val instanceof Int ? val.getValue() : val));
};

Int.prototype.remainder = function(val) {
    return new Int(this.value.mod(val instanceof Int ? val.getValue() : val));
};

Int.prototype.equals = function(val) {
    return this.value.equals((val instanceof Int || val instanceof Float) ? val.getValue() : val);
};

Int.prototype.lessThan = function(val) {
    return this.value.lessThan((val instanceof Int || val instanceof Float) ? val.getValue() : val);
};

Int.prototype.lessThanOrEq = function(val) {
    return this.value.lessThanOrEqualTo((val instanceof Int || val instanceof Float) ? val.getValue() : val);
};

Int.prototype.greaterThan = function(val) {
    return this.value.greaterThan((val instanceof Int || val instanceof Float) ? val.getValue() : val);
};

Int.prototype.greaterThanOrEq = function(val) {
    return this.value.greaterThanOrEqualTo((val instanceof Int || val instanceof Float) ? val.getValue() : val);
};

Int.prototype.match = function(val) {
    if (isNaN(val) || !this.equals(val)) {
        return undefined;
    }
    return val;
};


if (typeof exports != "undefined") {
    exports.Int = Int;
}
