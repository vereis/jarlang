/**
 * Class constructor for representing BitStrings in Jarlang's runtime
 */
const BitString = (() => {
    return class BitString extends ErlangDatatype {
        constructor() {
            super();

            // Process arguments
            let tmp = [];
            const args = [...arguments];
            const max8Bit = (1 << 8) - 1;
            args.forEach((arg) => {
                if (!Int.isInt(arg) && !List.isString(arg)) {
                    throw `BitString: Bad argument ${arg}`;
                }

                if (List.isString(arg)) {
                    arg.toString().split("").forEach((c) => tmp.push(c.charCodeAt(0)));
                }
                else {
                    tmp.push(arg.greaterThan(max8bit) ? max8bit : arg.getValue().c[0]);
                }
            });

            this.value = Uint8Array.of(...tmp);
        }

        toString() {
            return `<<${this.getValue.join(",")}>>`;
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
    }
})();