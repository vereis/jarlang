class ImproperList {
    constructor(value) {
        this.value = value;
    }

    toString() {
        return this.value;
    }

    static isImproperList(list) {
        return list instanceof ImproperList;
    }
}

class List {
    constructor(Head, ...Tail) {
        this.iterator = this;        
        this.value = Head;
        this.next = Tail.length ? new List(...Tail) : null;
    }

    toString() {
        let buffer = "";
        let reference = this;

        while (!!reference) {
            let seg = "";
            if (List.isList(reference)) {
                seg = reference.value;
            }
            else {
                seg = reference;
            }

            if (buffer.length) {
                buffer += (List.isList(reference) && !List.isImproperList(reference.value) ? ", " : " | ") + seg;
            }
            else {
                buffer += seg;
            }

            reference = reference.next;
        }

        return "L[" + buffer + "]";
    }

    cons(X) {
        let constructedList = List.cloneList(this);
        
        // Cons is technically only ever a function of the last entry of a clone of the current list,
        // so make sure it exists to deal with broken lists
        let last = constructedList.last();

        console.log(last, last.value);
        if (List.isImproperList(last.value)) {
            throw `Syntax Error: Cannot construct a new list with improper list '${constructedList.toString()}'`;
        }

        last.next = List.isList(X) ? X : new ImproperList(X);
        return constructedList;
    }

    nth(n) {
        if (n >= 0) {
            let reference = this;
            while (n > 0) {
                if (!reference.next) { throw `Exception Error: Index out of bounds`; }                
                reference = reference.next;
                n--;
            }
            return List.isList(reference) ? reference.value : reference;
        }
        else {
            throw `Exception Error: Negative indexes such as: ${n} are not allowed as argument for function Nth()`;
        }
    }

    last() {
        let reference = this;
        while (reference.next) {
            reference = reference.next;
        }

        return reference;
    }

    static isList(list) {
        return list instanceof List;
    }

    static isImproperList(list) {
        return list instanceof ImproperList;
    }

    static cloneList(list) {
        return new List(...list);
    }

    static isEmptyList(list) {
        return (list === undefined || list === null) || (List.isList(list) && list.length === 0); 
    }

    [Symbol.iterator]() {
        return {
            next: () => {
                if (!!this.iterator) {
                    let value = List.isImproperList(this.iterator) ? new ImproperList(this.iterator.value) : this.iterator.value;
                    this.iterator = this.iterator.next;
                    return {
                        value: value, 
                        done: false
                    };
                } 
                else {
                    this.iterator = this;
                    return {
                        done: true
                    };
                }
            }
        }
    }
}