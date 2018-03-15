/**
 *  Class constructor for representing atoms
 *  Defining atoms also adds the atom to a global atoms table in JRTS
 */

const Atom = (() => {
    // Private registerAtom token
    const registerAtom = Symbol("registerAtom");

    return class Atom extends ErlangDatatype {
        constructor(name) {
            super();
            this.value = name;
            this.precedence = 1;

            this[registerAtom]();
        }

        match(other) {
            if (other===null||(Atom.isAtom(other) ? this.getValue() === other.getValue() : this.getValue() === other)) {
                return other;
            }
            else {
                return undefined;
            }
        }

        toString() {
            return `${this.getValue()}`;
        }

        [registerAtom]() {
            try {
                jrts.atoms[name] = this;
            }
            catch (e) {
                console.warn(`Atom '${this.getValue()}' could not be registered to runtime atom table as it doesn't seem to exist`);
            }
        }

        static isAtom(a) {
            return a instanceof Atom;
        }

        static cloneAtom(a) {
            return a;
        }

        static exists(name) {
            try {
                return !!jrts.atoms[a];
            }
            catch (e) {
                console.warn(`Could not check if Atom '${name}' exists in atom table as atom table doesn't seem to exist`);
                return false;
            }
        }
    };
})();
