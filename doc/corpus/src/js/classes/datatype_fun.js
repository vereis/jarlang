/**
 * Class constructor for representing Erlang funs in Jarlang's runtime
 */

const Fun = (() => {
    return class Fun extends ErlangDatatype {
        constructor() {
            super();
            this.precedence = 3;
        }

        getValue() {
            throw "Fun is not implemented yet";
        }

        toString() {
            throw "Fun is not implemented yet";
        }

        match(other) {
            throw "Fun is not implemented yet";
        }

        static isFun(a) {
            return a instanceof Fun;
        }
    };
})();