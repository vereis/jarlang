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
        }

        toString() {
            return "#Ref<"+this.value.toString()+">";
        }

        match(x) {
            if (Reference.isReference(x) && this.value === x.value) {
                return x;
            }
            else {
                return undefined;
            }
        };

        static isReference(r) {
            return r instanceof Reference;
        }
    }
})();