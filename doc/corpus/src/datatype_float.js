/**
 * Class constructor for representing Erlang floats in Jarlang's runtime
 */

const Float = (() => {
    return class Float extends ErlangDatatype {
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
            return new Float(this.value.plus((val instanceof Int || val instanceof Float) ? val.getValue() : val));
        }
        
        subtract(val) {
            return new Float(this.value.minus((val instanceof Int || val instanceof Float) ? val.getValue() : val));
        }
        
        multiply(val) {
            return new Float(this.value.times((val instanceof Int || val instanceof Float) ? val.getValue() : val));
        }
        
        divide(val) {
            return new Float(this.value.div((val instanceof Int || val instanceof Float) ? val.getValue() : val));
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

        static isFloat(a) {
            return a instanceof Float;
        }
        
        static cloneFloat(n) {
            return new Float(this.getValue());
        }
    };
})();