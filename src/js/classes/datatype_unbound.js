/**
 * Class constructor for representing Erlang Unbounds in Jarlang's runtime
 */

const Unbound = (() => {
    return class Unbound extends ErlangDatatype {
        constructor() {
            super();
            this.value = ++refCount;
        }

        toString() {
            return "unbound";
        }

        match(x) {
            return x;
        }

        static isUnbound(x) {
            return x instanceof Unbound;
        }
    };
})();