// Constructor
function Tuple (car, ...cdr) {
    this.val = car;
    this.next = car !== undefined ? new Tuple(...cdr) : undefined;

    this.iterator = this;
}


// Static Methods
Tuple.isTuple = (tuple) => tuple instanceof Tuple;
Tuple.isEmptyTuple = (tuple) => Tuple.isTuple(tuple) && tuple.val === undefined && tuple.next === undefined;
Tuple.cloneTuple = (tuple) => new Tuple(...tuple);


// Internal Usage Prototype Methods
Tuple.prototype.__nthNode = function(n) {
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
};

Tuple.prototype[Symbol.iterator] = function() {
    return {
        next: () => {
            //console.log(this, this.iterator);
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
};

Tuple.prototype.__last = function() {
    return this.__nthNode(this.size() - 1);
};


// Prototype Methods
Tuple.prototype.nth = function(n) {
    let nth = this.__nthNode(n);
    return Tuple.isTuple(nth) ? nth.val : nth;
};

Tuple.prototype.size = function() {
    return [...this].length;
};

Tuple.prototype.value = function() {
    return [...this];  
};

Tuple.prototype.toString = function() {
    return `{${[...this].join(",")}}`;
};


if (typeof exports != "undefined") {
    exports.Tuple = Tuple;
}
