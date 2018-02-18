/**
 * Class constructor for representing Erlang ints in Jarlang's runtime
 */

const Int = (() => {
    return class Int extends ErlangDatatype {
        constructor(val) {
            super();
            this.value = val instanceof BigNumber ? val : new BigNumber(val);
        }

        toString() {
            return this.getValue().toString();
        }

        match(other) {
            if (!isNaN(other) && this.equals(other)) {
                return other;
            }
            else {
                return undefined;
            }
        }

        add(val) {
            return new Int(this.value.plus((val instanceof Int || val instanceof Float) ? val.getValue() : val));
        }
        
        subtract(val) {
            return new Int(this.value.minus((val instanceof Int || val instanceof Float) ? val.getValue() : val));
        }
        
        multiply(val) {
            return new Int(this.value.times((val instanceof Int || val instanceof Float) ? val.getValue() : val));
        }
        
        divide(val) {
            return new Int(this.value.div((val instanceof Int || val instanceof Float) ? val.getValue() : val));
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