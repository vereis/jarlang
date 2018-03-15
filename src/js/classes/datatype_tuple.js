/**
 * Class constructor for representing Erlang Tuples in Jarlang's runtime
 */

const Tuple = (() => {
    // Private nthNode token
    const nthNode = Symbol("nthNode");

    // Private last token
    const last = Symbol("last");

    return class Tuple extends ErlangDatatype {
        constructor(car, ...cdr) {
            super();
            this.val = car;
            this.next = car !== undefined ? new Tuple(...cdr) : undefined;
        
            this.iterator = this;
            this.precedence = 6;
        }

        nth(n) {
            let nth = this[nthNode](n);
            return Tuple.isTuple(nth) ? nth.val : nth;
        }
        
        size() {
            return [...this].length;
        }
        
        value() {
            return [...this];  
        }
        
        toString() {
            return `{${[...this].join(",")}}`;
        }

        [nthNode](n) {
            if (n < 0 || n >= this.size()) {
                throw "index out of bounds error";
            }    
        
            let i = 0;
            let walker = this;
        
            while (i < n) {
                walker = walker.next;
                i++;
            }
        
            return walker;
        }

        [Symbol.iterator]() {
            return {
                next: () => {
                    let v = this.iterator.val;
                    
                    if (this.iterator === undefined || Tuple.isEmptyTuple(this.iterator)) {
                        this.iterator = this;
                        return {                    
                            done: true
                        };
                    }
                    else {
                        this.iterator = this.iterator.next;
                        return {
                            value: v,
                            done: false
                        };
                    }
                }
            };
        }

        [last]() {
            return this.size() === 0 ? this : this[nthNode](this.size() - 1);
        }

        static isTuple(t) {
            return t instanceof Tuple;
        }

        static isEmptyTuple(tuple) {
            return Tuple.isTuple(tuple) && tuple.val === undefined && tuple.next === undefined;
        }

        static cloneTuple(tuple) {
            return new Tuple(...tuple);
        }

        match(other) {
            if(other===null)return other;
            if (List.isTuple(other) && this.size() === other.size()) {
                for (let i = 0; i < this.size(); i++) {
                    if (ErlangDatatype.isErlangDatatype(this.nth(i))) {
                        if (this.nth(i).match(other.nth(i)) === undefined) {
                            return undefined;
                        }
                    }
                    else {
                        if (this.nth(i) !== other.nth(i) && other.nth(i) !== null) {
                            return undefined;
                        }
                    }
                }
                return other;
            }
            else {
                return undefined;
            }
        }
    };
})();