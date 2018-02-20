/**
 * Class constructor for representing Erlang maps in Jarlang's runtime
 */

const Map = (() => {
    return class Map extends ErlangDatatype {
        constructor(val) {
            super();
            this.value = (val.constructor.name === "Object") ? val : {};
            this.precedence = 7;
        }

        get(key) {
            if (typeof key !== "string") {
                key = JSON.stringify(key);
            }
        
            return this.getValue()[key];
        }
        
        put(key, value) {
            if (typeof key !== "string") {
                key = JSON.stringify(key);
            }
        
            this.value[key] = value;
        }
        
        update(key, value) {
            if (typeof key !== "string") {
                key = JSON.stringify(key);
            }
        
            if (this.value[key]) {
                this.value[key] = value;
            }
        }
        
        remove(key) {
            if (typeof key !== "string") {
                key = JSON.stringify(key);
            }
        
            delete this.value[key];
        }
        
        size() {
            var size = 0, 
                k;
        
            for (k in this.value) {
                if (this.value.hasOwnProperty(k)) {
                    size++;
                }
            }
        
            return size;
        }
        
        // todo: Ensure keys are ordered as they are in erlang
        toString() {
            var pairs = [], k;
        
            for (let k in this.value) {
                if (this.value.hasOwnProperty(k)) {
                    if (Map.isMap(this.value[k])) {
                        pairs.push(k + "=>" + this.value[k].toString());
                    } else {
                        pairs.push(k + "=>" + JSON.stringify(this.value[k]));
                    }
                }
            }
        
            return "#{" + pairs.join(",") + "}";
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