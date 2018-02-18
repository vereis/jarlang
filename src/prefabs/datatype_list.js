// Constructor
function List (car, ...cdr) {
    if (typeof car == "string") {
        var chars = car.split("");

        for (var i = 0; i < chars.length; i++) {
            chars[i] = chars[i].charCodeAt(0);
        }

        this.value = new List(...chars);
    }
    else {
        this.value = car;
    }

    this.next = car !== undefined ? new List(...cdr) : undefined;
    this.iterator = this;
}


// Static Methods
List.isList = (list) => list instanceof List;

List.isEmptyList = (list) => List.isList(list) && list.value === undefined && list.next === undefined;

List.isString = (list) => {
    if (!List.isList(list)) {
        return false;
    }

    for (var i = 0; i < list.size(); i++) {
        if (!isLatin1Char(list.nth(i))) {
            return false;
        }
    }
    return true;
};

List.cloneList = (list) => new List(...list);


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
            let v = List.isList(this.iterator) ? this.iterator.value : this.iterator;
            
            if (this.iterator === "done" || List.isEmptyList(this)) {
                this.iterator = this;
                return {                    
                    done: true
                };
            }
            else {
                this.iterator = isLastNode ? "done" : this.iterator.next;
                return {
                    value: v,
                    done: false
                };
            }
        }
    };
};

List.prototype.__last = function() {
    return this.__nthNode(this.size() - 1);
};


// Prototype Methods
List.prototype.nth = function(n) {
    let nth = this.__nthNode(n);
    return List.isList(nth) ? nth.value : nth;
};

List.prototype.size = function() {
    return [...this].length;
};

List.prototype.cons = function(appendage) {
    let clone = List.cloneList(this);
    clone.__last().next = appendage;

    return clone;
};

List.prototype.getValue = function() {
    return [...this];  
};

List.prototype.toString = function() {
    let buffer = [...this];

    if (buffer.length) {
        let textBuffer = "";
        let isString = List.isString(this);
        let isImproperList = !List.isList(this.__nthNode(Math.max(0, buffer.length - 1)));
        
        for (let i = 0; i < buffer.length; i++) {
            if (!isString) {
                if (i > 0) {
                    if (i === buffer.length - 1 && isImproperList) {
                        textBuffer += "|";
                    }
                    else {
                        textBuffer += ",";
                    }
                }

                textBuffer += buffer[i];
            } else {
                textBuffer += String.fromCharCode(buffer[i]);
            }
        }

        return isString ? textBuffer : `[${textBuffer}]`;
    }
    
    return '[]';
};


// Private Methods
function isLatin1Char(c) {
    if (!Number.isInteger(c)) {
        return false;
    }
    return (c >= 32 && c <= 126) || (c >= 160 && c <= 255);
}


if (typeof exports != "undefined") {
    exports.List = List;
}
