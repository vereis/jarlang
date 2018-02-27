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
            const segmentSizeLimit = 1 << 8;

            args.forEach((arg) => {
                if (typeof arg == "string" || List.isString(arg)) {
                    arg.toString().split("").forEach((c) => this.value.push(new Int(c.charCodeAt(0))));
                }
                else if (Number.isInteger(arg)) {
                    this.value.push(new Int(arg % segmentSizeLimit));
                }
                else if (Int.isInt(arg)) {
                    this.value.push(arg.remainder(segmentSizeLimit));
                }
                else {
                    throw `BitString: Bad argument ${arg}`;
                }
            });

        }

        toString() {
            var l = new List(...this.getValue());

            if (List.isString(l)) {
                return `<<"${l}">>`;
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
