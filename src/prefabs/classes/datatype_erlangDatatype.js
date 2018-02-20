/**
 *  Class constructor for representing generic erlang datatypes
 *  This is intended to be inherited by other erlang datatype classes
 */

const ErlangDatatype = (() => {
    return class ErlangDatatype {
        constructor() {
            this.value = 0;
            this.precedence = 0;
        }

        getValue() {
            return this.value;
        }

        toString() {
            return this.value.toString();
        }

        match(other) {
            if (ErlangDatatype.isErlangDatatype(other) ? this.getValue() === other.getValue() : this.getValue() === other) {
                return other;
            }
            else {
                return undefined;
            }
        }

        isUnbound() {
            return false;
        }

        getComparator() {
            return this.precedence;
        }

        static isErlangDatatype(a) {
            return a instanceof ErlangDatatype;
        }
    };
})();