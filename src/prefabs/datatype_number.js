var BigNumber = require("bignumber.js");

class ErlNumber {
    constructor(value) {
        this.value = new BigNumber(value);
    }

    toString() {
        return this.value.toString();
    }

    static isErlNumber(erlnum) {
        return erlnum instanceof ErlNumber;
    }

    static cloneNumber(erlnum) {
        return new ErlNumber(erlnum.toString());
    }

    isUnbound() {
        return false;
    }

    match(value) {
        if (!this.value.equals(value)) {
            return undefined;
        }
        return value;
    }
}

exports.ErlNumber = ErlNumber;
