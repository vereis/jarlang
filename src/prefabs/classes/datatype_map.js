/**
 * Class constructor for representing Erlang maps in Jarlang's runtime
 */

const Map = (() => {
    return class Map extends ErlangDatatype {
        constructor(keys, values) {
            super();

            if (Array.isArray(keys)) {
                keys = new List(...keys);
            }
            if (Array.isArray(values)) {
                values = new List(...values);
            }

            this.value = {
                keys: keys,
                values: values
            };
            this.precedence = 7;
        }

        get(key) {
            if (key instanceof ErlangDatatype) {
                for (let i = 0; i < this.getValue().keys.size(); i++) {
                    if (key.match(this.getValue().keys.nth(i))) {
                        return this.getValue().values.nth(i);
                    }
                }
            }
            throw "bad key"; //todo
        }

        put(key, value) {
            this.value.keys = new List(...this.getValue().keys, key);
            this.value.values = new List(...this.getValue().values, value);
        }

        update(key, value) {
            if (key instanceof ErlangDatatype) {
                for (let i = 0; i < this.getValue().keys.size(); i++) {
                    if (key.match(this.getValue().keys.nth(i))) {
                        let tmp = [...this.getValue().values];
                        tmp.splice(i, 1, value);
                        this.value.values = new List(...tmp);
                        return;
                    }
                }
            }
            throw "bad key"; //todo
        }

        remove(key) {
            if (key instanceof ErlangDatatype) {
                for (let i = 0; i < this.getValue().keys.size(); i++) {
                    if (key.match(this.getValue().keys.nth(i))) {
                        let tmp1 = [...this.getValue().keys];
                        tmp1.splice(i, 1);

                        let tmp2 = [...this.getValue().values];
                        tmp2.splice(i, 1);

                        this.value.keys = new List(...tmp1);
                        this.value.values = new List(...tmp2);
                        return;
                    }
                }
            }
        }
        
        size() {
            return this.getValue().keys.size();
        }
        
        // todo: Ensure keys are ordered as they are in erlang
        toString() {
            let k;
            let pairs = [];
            let tmp1 = [...this.getValue().keys];
            let tmp2 = [...this.getValue().values];

            while (k = tmp1.shift()) {
                pairs.push(`${k}=>${tmp2.shift()}`);
            }
            return `#{${pairs.sort().join(", ")}}`;
        }
        
        // TODO: actually implement mapping properly
        match(map) {
            if (Map.isMap(map) && this.equals(map.getValue())) {
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
