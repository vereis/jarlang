// Constructor
function List (car, ...cdr) {
    this.val = car;
    this.next = car !== undefined ? new List(...cdr) : undefined;

    this.iterator = this;
};


// Static Methods
List.isList = (list) => list instanceof List;
List.isEmptyList = (list) => List.isList(list) && list.value === undefined && list.next === undefined;
List.clonList = (list) => new List(...list);


// Internal Usage Prototype Methods
List.prototype.__nthNode = function(n) {
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

List.prototype[Symbol.iterator] = function() {
    return {
        next: () => {
            // If the next node of the current iterator isn't another list OR is an empty list, then we know
            // we have reached the end of the linked list
            let isLastNode = this.iterator.next === undefined || List.isEmptyList(this.iterator.next);
            let v = List.isList(this.iterator) ? this.iterator.val : this.iterator;
            
            if (this.iterator === "done") {
                return {                    
                    done: true
                }
            }
            else {
                this.iterator = isLastNode ? "done" : this.iterator.next;
                return {
                    value: v,
                    done: false
                };
            }
        }
    }
};

List.prototype.__last = function() {
    return this.__nthNode(this.size() - 1);
};


// Prototype Methods
List.prototype.nth = function(n) {
    let nth = this.__nthNode(n);
    return List.isList(nth) ? nth.val : nth;
};

List.prototype.size = function() {
    let i = 1;
    let walker = this;

    while (List.isList(walker) && !List.isEmptyList(walker.next) && walker.next !== undefined) {
        walker = walker.next;
        i++;
    } 

    return i;
};

List.prototype.cons = function(appendage) {
    let clone = List.clonList(this);
    clone.__last().next = appendage;

    return clone;
};

List.prototype.values = function() {
    let walker = this;
    let values = [this.val];

    while (List.isList(walker) && !List.isEmptyList(walker.next) && walker.next !== undefined) {
        walker = walker.next;
        values.push(List.isList(walker) ? walker.val : walker);
    } 

    return values;  
};