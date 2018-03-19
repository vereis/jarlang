/**
 * Class constructor for representing Erlang maps in Jarlang's runtime
 */

const Map = (() => {
    return class Map extends ErlangDatatype {
        constructor(keys, values) {
            super();

            this.value = {
                keys: keys,
                values: values
            };
            this.precedence = 7;
        }

        [Symbol.iterator]() {
            let k,
            tmp1 = [...this.getValue().keys],
            tmp2 = [...this.getValue().values];

            return {
                next: () => {
                    if ((k = tmp1.shift())) {
                        return {
                            value: new Tuple(k, tmp2.shift()),
                            done: false
                        };
                    }
                    return {
                        done: true
                    };
                }
            };
        }

        get(key) {
            if (key instanceof ErlangDatatype) {
                for (let i = 0; i < this.getValue().keys.length; i++) {
                    if (key.match(this.getValue().keys[i])) {
                        return this.getValue().values[i];
                    }
                }
            }
            throw "bad key"; //todo
        }

        put(key, value) {
            this.value.keys.push(key);
            this.value.values.push(value);
        }

        update(key, value) {
            if (key instanceof ErlangDatatype) {
                for (let i = 0; i < this.getValue().keys.length; i++) {
                    if (key.match(this.getValue().keys[i])) {
                        this.value.values.splice(i, 1, value);
                        return;
                    }
                }
            }
            throw "bad key"; //todo
        }

        remove(key) {
            if (key instanceof ErlangDatatype) {
                for (let i = 0; i < this.getValue().keys.length; i++) {
                    if (key.match(this.getValue().keys[i])) {
                        this.value.keys.splice(i, 1);
                        this.value.values.splice(i, 1);
                        return;
                    }
                }
            }
        }

        size() {
            return this.getValue().keys.length;
        }

        // todo: Ensure keys are ordered as they are in erlang
        toString() {
            let k, pairs = [],
            tmp1 = [...this.getValue().keys],
            tmp2 = [...this.getValue().values];

            while ((k = tmp1.shift())) {
                pairs.push(`${k}=>${tmp2.shift()}`);
            }
            return `#{${pairs.sort().join(", ")}}`;
        }

        // TODO: actually implement mapping properly
        match(map) {
            if (map===null || (Map.isMap(map) && this.equals(map.getValue()))) {
                return map;
            } else {
                return undefined;
            }
        }

        static isMap(m) {
            return m instanceof Map;
        }
    };
})();
