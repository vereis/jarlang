/**
 * Class constructor for representing BitStrings in Jarlang's runtime
 */

const BitString = (() => {
    return class BitString extends ErlangDatatype {
        constructor() {
            super();

            this.precedence = 10;
            this.value = {
                sizes: []
            };

            // Process arguments
            const args = [...arguments];

            var tmp = [], arg;

            while (arg = args.shift()) {
                if (typeof arg == "string" || List.isString(arg)) {
                    arg.toString().split("").forEach((c) => {
                        tmp.push(c.charCodeAt(0));
                        this.value.sizes.push(8);
                    });
                }
                else {
                    if (!Array.isArray(arg)) {
                        arg = [arg];
                    }

                    if (!Number.isInteger(arg[0])) {
                        throw `BitString: Bad argument ${arg[0]}`;
                    }

                    if (1 in arg && arg[1] !== 8) {
                        if (arg[1] > 8*2) {
                            tmp.push(0);
                            this.value.sizes.push(8);
                            args.unshift([arg[0], [arg[1] - 8]]);
                        }
                        else if (arg[1] > 8) {
                            tmp.push(arg[0] >> (arg[1] - 8));
                            this.value.sizes.push(8);
                            args.unshift([arg[0], arg[1] - 8]);
                        }
                        else if (arg[1] >= 1 && arg[1] < 8) {
                            tmp.push(Math.min(arg[0], (1 << arg[1]) - 1));
                            this.value.sizes.push(arg[1]);
                        }
                        else {
                            throw `BitString: Bad argument ${arg[1]}`;
                        }
                    }
                    else {
                        this.value.sizes.push(8);
                    }
                }
            }

            this.value.values = Uint8Array.of(...tmp);
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
