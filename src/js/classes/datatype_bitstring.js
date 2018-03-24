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
            var vals = new List(...this.getValue().values);

            if (BitString.isBinary(this)) {
                if (List.isString(vals)) {
                    return `<<"${vals}">>`;
                }
                return `<<${this.getValue().values.join(",")}>>`;
            }

            vals = [...vals];
            var sizes = [...this.getValue().sizes], tmp = [], i;

            while (i = sizes.shift()) {
                if (i !== 8) {
                    tmp.push(`${vals.shift()}:${i}`);
                }
                else {
                    tmp.push(vals.shift());
                }
            }

            return `<<${tmp.join(",")}>>`;
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
        
        static isBinary(a) {
            return a instanceof BitString && a.getValue().sizes.every((s) => s === 8);
        }
    };
})();
