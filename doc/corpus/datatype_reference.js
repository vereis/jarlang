/**
 * Class constructor for representing Erlang References in Jarlang's runtime
 */

const Reference = (() => {
    // Private last token
    let refCount = 0;

    return class Reference extends ErlangDatatype {
        constructor() {
            super();
            this.value = ++refCount;
            this.precedence = 2;
        }

        toString() {
            return "#Ref<"+this.value.toString()+">";
        }

        match(x) {
            if (x===null || (Reference.isReference(x) && this.value === x.value)) {
                return x;
            }
            else {
                return undefined;
            }
        }

        static isReference(r) {
            return r instanceof Reference;
        }
    };
})();