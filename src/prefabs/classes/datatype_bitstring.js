/**
 * Class constructor for representing BitStrings in Jarlang's runtime
 */

const BitString = (() => {
    return class BitString extends ErlangDatatype {
        constructor() {
            super();

            this.value = [];
            this.precedence = 10;

            // Process arguments
            const args = [...arguments];

            args.forEach((arg) => {
                if (!Int.isInt(arg) && !List.isString(arg)) {
                    throw `BitString: Bad argument ${arg}`;
                }

                if (List.isString(arg)) {
                    arg.toString().split("").forEach((c) => this.value.push(c.charCodeAt(0)));
                }
                else {
                    this.value.push(arg.remainder(1 << 8));
                }
            });

        }

        toString() {
            var l = new List(...this.getValue());

            if (List.isString(l)) {
                return `<<"${l.toString()}">>`;
            }
            return `<<${this.getValue().join(",")}>>`;
        }

        match(other) {
            if (BitString.isBitString(other) && this.value === other.value) {
                return other;
            }
            else {
                return undefined;
            }
        }

        static isBitString(a) {
            return a instanceof BitString;
        }
        
        // TODO
        static isBinary(a) {
            return a instanceof Bitstring;
        }
    };
})();
