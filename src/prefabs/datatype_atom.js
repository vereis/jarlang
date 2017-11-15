class Atom {
    constructor(value) {
        this.value = value;
    }

    toString() {
        return value.toString();
    }

    static isAtom(atom) {
        return atom instanceof Atom;
    }

    static cloneAtom(atom) {
        return atom;
    }

    isUnbound(){
        return false;
    }

    match(Var){
        if(!Atom.isAtom(Var) || !value.equals(Var))return undefined;
        return Var;
    }
}
