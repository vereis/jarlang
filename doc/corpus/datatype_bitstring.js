/**
 * Class constructor for representing BitStrings in Jarlang's runtime
 */

const defaultSize = 8;

const BitString = (() => {
    return class BitString extends ErlangDatatype {
        constructor() {
            super();

            this.precedence = 10;
            this.value = {
                values: [],
                sizes: []
            };

            // Process arguments
            const args = [...arguments];
            var arg;

            while ((arg = args.shift()) !== undefined) {
                if (typeof arg == "string" || List.isString(arg)) {
                    arg.toString().split("").forEach((c) => {
                        this.value.values.push(c.charCodeAt(0));
                        this.value.sizes.push(defaultSize);
                    });
                }
                else if (arg[1] !== 0) {
                    if (!Array.isArray(arg)) {
                        arg = [arg];
                    }

                    if (!Number.isInteger(arg[0])) {
                        throw `BitString: Bad argument ${arg[0]}`;
                    }

                    if (1 in arg && arg[1] !== defaultSize) {
                        if (!Number.isInteger(arg[1]) || arg[1] < 0) {
                            throw `BitString: Bad argument ${arg[1]}`;
                        }

                        if (arg[1] > defaultSize) {
                            this.value.values.push(arg[1] > defaultSize * 2 ? 0 :
                                arg[0] >> (arg[1] - defaultSize));
                            this.value.sizes.push(defaultSize);
                            args.unshift([arg[0], arg[1] - defaultSize]);
                        }
                        else {
                            this.value.values.push(arg[0] % (1 << arg[1]));
                            this.value.sizes.push(arg[1]);
                        }
                    }
                    else {
                        this.value.values.push(arg[0] % (1 << defaultSize));
                        this.value.sizes.push(defaultSize);
                    }
                }
            }
        }

        [Symbol.iterator]() {
            var tmp = [...this.getValue().values], v;

            return {
                next: () => {
                    if ((v = tmp.shift()) !== undefined) {
                        return {
                            value: v,
                            done: false
                        };
                    }
                    return {
                        done: true
                    };
                }
            }
        }

        toString() {
            if (!this.getValue().values.length) {
                return "<<>>";
            }

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
                if (i !== defaultSize) {
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
            return a instanceof BitString && a.getValue().sizes.every((s) => s === defaultSize);
        }
    };
})();
