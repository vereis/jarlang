/**
 * Class constructor for representing Erlang lists in Jarlang's runtime
 */

const List = (() => {
    // Private nthNode token
    const nthNode = Symbol("nthNode");

    // Private last token
    const last = Symbol("last");

    // Private isLatin1Char token
    const isLatin1Char = Symbol("isLatin1Char");

    return class List extends ErlangDatatype {
        constructor(car, ...cdr) {
            super();
            if (typeof car === "string") {
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
            this.precedence = 9;
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

        [last]() {
            return this.size() === 0 ? this : this[nthNode](this.size() - 1);
        }

        [Symbol.iterator]() {
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
        }

        nth(n) {
            let nth = this[nthNode](n);
            return List.isList(nth) ? nth.value : nth;
        }

        size() {
            return [...this].length;
        }

        cons(appendage) {
            let clone = List.cloneList(this);
            clone[last]().next = appendage;
            return clone;
        }

        getValue() {
            return [...this];
        }

        toString() {
            let buffer = [...this];

            if (buffer.length) {
                let textBuffer = "";
                let isString = List.isString(this);
                let isImproperList = !List.isList(this[nthNode](Math.max(0, buffer.length - 1)));
                
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
        }

        match(other) {
            if (List.isList(other) && this.size() === other.size()) {
                for (let i = 0; i < this.size(); i++) {
                    if (ErlangDatatype.isErlangDatatype(this.nth(i)) && ErlangDatatype.isErlangDatatype(other.nth(i))) {
                        if (this.nth(i).match(other.nth(i)) === undefined) {
                            return undefined;
                        }
                    }
                    else {
                        if (this.nth(i) !== other.nth(i)) {
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

        static isList(a) {
            return a instanceof List;
        }

        static isEmptyList(a) {
            return List.isList(a) && a.value === undefined && a.next === undefined;
        }

        static [isLatin1Char](c) {
            if (Number.isInteger(c)) {
                return (c >= 32 && c <= 126) || (c >= 160 && c <= 255) || (c === 10);
            }

            if (Int.isInt(c)) {
                return (c.greaterThanOrEq(32) && c.lessThanOrEq(126)) || (c.greaterThanOrEq(160) && c.lessThanOrEq(255)) || (c.equals(10));
            }
            return false;
        }

        static isString(a) {
            if (!List.isList(a)) {
                return false;
            }
        
            for (var i = 0; i < a.size(); i++) {
                if (!List[isLatin1Char](a.nth(i))) {
                    return false;
                }
            }
            return true;
        }

        static cloneList(a) {
            return new List(...a);
        }
    };
})();
