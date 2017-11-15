class Unbound {
    constructor() {
    }

    toString() {
        throw `variable is unbound`;
    }
    
    isUnbound(){
        return true;
    }
    
    match(Var){
        return Var;
    }
}
