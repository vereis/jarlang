/**
 * Class constructor for representing Erlang ports in Jarlang's runtime
 */

const Port = (() => {
    return class Port extends ErlangDatatype {
        constructor() {
            super();
            this.precedence = 4;
        }

        getValue() {
            throw "Port is not implemented yet";
        }

        toString() {
            throw "Port is not implemented yet";
        }

        match(other) {
            throw "Port is not implemented yet";
        }

        static isPort(a) {
            return a instanceof Port;
        }
    };
})();