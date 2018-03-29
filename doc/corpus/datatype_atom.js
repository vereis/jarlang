/**
 *  Class constructor for representing atoms
 *  Defining atoms also adds the atom to a global atoms table in JRTS
 */

const Atom = (() => {
    // Private registerAtom token
    const registerAtom = Symbol("registerAtom");
    const registered   = Symbol("registered");

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

        registerPid(p) {
            if (Pid.isPid(p)) {
                if (jrts.atoms[this.value] === registered) {
                    jrts.atoms[this.value] = p;
                }
                else {
                    throw "** exception error: this atom has already been registered to a different Pid";
                }
            }
            else {
                throw "** exception error: you can only register a Pid to an Atom with a Pid";
            }
        }

        [registerAtom]() {
            try {
                if (!jrts.atoms[this.value]) {
                    jrts.atoms[this.value] = registered;
                }
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
