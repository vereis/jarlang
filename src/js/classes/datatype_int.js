/**
 * Class constructor for representing Erlang ints in Jarlang's runtime
 */

const Int = (() => {
    return class Int extends ErlangDatatype {
        constructor(val) {
            super();
            this.value = val instanceof BigNumber ? val : new BigNumber(val);
            this.precedence = 0;
        }

        toString() {
            return this.getValue().toString();
        }

        match(other) {
            if (other===null||(!isNaN(other) && this.equals(other))) {
                return other;
            }
            else {
                return undefined;
            }
        }

        add(val) {
            if (val instanceof Float) {
                return new Float(this.value.plus(val.getValue()));
            }
            return new Int(this.value.plus(val instanceof Int ? val.getValue() : val));
        }

        subtract(val) {
            if (val instanceof Float) {
                return new Float(this.value.minus(val.getValue()));
            }
            return new Int(this.value.minus(val instanceof Int ? val.getValue() : val));
        }

        multiply(val) {
            if (val instanceof Float) {
                return new Float(this.value.times(val.getValue()));
            }
            return new Int(this.value.times(val instanceof Int ? val.getValue() : val));
        }

        divide(val) {
            if (val instanceof Float || !this.remainder(val).equals(0)) {
                return new Float(this.value.div((val instanceof Int || val instanceof Float) ? val.getValue() : val));
            }
            return new Int(this.value.div(val instanceof Int ? val.getValue() : val));
        }

        intDivide(val) {
            return new Int(this.value.divToInt(val instanceof Int ? val.getValue() : val));
        }

        remainder(val) {
            return new Int(this.value.mod(val instanceof Int ? val.getValue() : val));
        }

        equals(val) {
            return this.value.equals((val instanceof Int || val instanceof Float) ? val.getValue() : val);
        }

        lessThan(val) {
            return this.value.lessThan((val instanceof Int || val instanceof Float) ? val.getValue() : val);
        }

        lessThanOrEq(val) {
            return this.value.lessThanOrEqualTo((val instanceof Int || val instanceof Float) ? val.getValue() : val);
        }

        greaterThan(val) {
            return this.value.greaterThan((val instanceof Int || val instanceof Float) ? val.getValue() : val);
        }

        greaterThanOrEq(val) {
            return this.value.greaterThanOrEqualTo((val instanceof Int || val instanceof Float) ? val.getValue() : val);
        }

        static isInt(a) {
            return a instanceof Int;
        }

        static cloneInt(n) {
            return new Int(this.getValue());
        }
    };
})();
