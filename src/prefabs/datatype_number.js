class Number {
    constructor(value) {
        this.value = value;
    }

    toString() {
        return value.toString();
    }

    static isNumber(number) {
        return number instanceof Number;
    }

    static cloneNumber(number) {
        return new Number(number);
    }

    isUnbound(){
        return false;
    }

    match(Var){
        if(!Number.isNumber(Var) || !value.equals(Var))return undefined;
        return Var;
    }
}
