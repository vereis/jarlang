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
            var v = new List(...this.getValue().values);

            if (isBinary(this)) {
                if (List.isString(v)) {
                    return `<<"${v}">>`;
                }
                return `<<${this.getValue().values.join(",")}>>`;
            }

            var tmp = [];

            for (i = 0; i < v.length; i++) {
                if (s[i] !== 8) {
                    tmp.push(`${v[i]}:${s[i]}`);
                }
                else {
                    tmp.push(v[i]);
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
        
        // TODO
        static isBinary(a) {
            return a instanceof BitString && a.getValue().sizes.every((s) => s === 8);
        }
    };
})();
