class Tuple {
    constructor(...Contents) {
        this.iterator = 0;
        this.length = Contents.length;        
        this.value = Contents;
    }

    toString() {
        let buffer = "";
        for (const elem of this) {
            if (buffer.length) {
                buffer += ", " + elem.toString();
            }
            else {
                buffer += elem.toString();                
            }
        }

        return "T{" + buffer + "}";
    }

    nth(n) {
        if (n >= 0) {
            if (n > this.length) {
                throw `Exception Error: Index out of bounds`;                
            }
            return this.value[n];
        }
        else {
            throw `Exception Error: Negative indexes such as: ${n} are not allowed as argument for function Nth()`;
        }
    }

    static isTuple(tuple) {
        return tuple instanceof Tuple;
    }

    static cloneTuple(tuple) {
        return new List(...tuple);
    }

    static isEmptyTuple(tuple) {
        return (tuple === undefined || tuple === null) || (Tuple.isTuple(tuple) && tuple.length === 0); 
    }

    [Symbol.iterator]() {
        return {
            next: () => {
                if (this.iterator < this.length) {
                    return {
                        value: this.value[this.iterator++], 
                        done: false
                    };
                } 
                else {
                    this.iterator = 0;
                    return {
                        done: true
                    };
                }
            }
        }
    }

    isUnbound(){
        return false;
    }

    match(Var){
        if(!Tuple.isTuple(Var) || length!=Var.length){return undefined;}//test if Var is a tuple and the same length
        var i=0;
        for(const elem of Var){
            if(value[i].match(elem) == undefined)return undefined;//test if any content is already bound to something else
        }
        return Var;
    }
}
