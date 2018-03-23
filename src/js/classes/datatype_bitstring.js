/**
 * Class constructor for representing BitStrings in Jarlang's runtime
 */

const BitString = (() => {
    return class BitString extends ErlangDatatype {
        constructor() {
            super();

            this.precedence = 10;

            // Process arguments
            const args = [...arguments];

            var tmp = [];

            args.forEach((arg) => {
                if (typeof arg[0] == "string" || List.isString(arg[0])) {
                    arg[0].toString().split("").forEach((c) => tmp.push(c.charCodeAt(0)));
                }
                else if (Number.isInteger(arg[0])) {
                    tmp.push(arg[0]);
                }
                else {
                    throw `BitString: Bad argument ${arg[0]}`;
                }
            });

            this.value = {
                values: Uint8Array.of(...tmp),
                sizes: [...args].map(x => 1 in x ? x[1] : 8)
            };
        }

        toString() {
            var l = new List(...this.getValue().values);

            if (List.isString(l)) {
                return `<<"${l}">>`;
            }
            return `<<${this.getValue().values.join(",")}>>`;
        }

        match(other) {
            if (other === null || (BitString.isBitString(other) && this.value === other.value)) {
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
            return a instanceof BitString;
        }
    };
})();
