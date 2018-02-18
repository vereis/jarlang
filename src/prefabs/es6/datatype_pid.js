/**
 * Class constructor for representing Erlang PIDs in Jarlang's runtime
 */

const Pid = (() => {
    // Private rng token
    const rng = Symbol("rng");

    return class Pid extends ErlangDatatype {
        constructor(arg) {
            super();
            if (arguments.length === 3) {
                this.node = arguments[0];
                this.id_a = arguments[1];
                this.id_b = arguments[2];
            }
            else if (arg !== undefined && typeof (arg) === "function") {
                let res = arg();
                if (Array.isArray(res) && res.length === 3) {
                    this.node = res[0];
                    this.id_a = res[1];
                    this.id_b = res[2];
                }
                else if (res.node && res.id_a && res.id_b) {
                    this.node = res.node;
                    this.id_a = res.id_a;
                    this.id_b = res.id_b;
                }
                else {
                    this.node = res.toString();
                    this.id_a = res.toString();
                    this.id_b = res.toString();
                }
            }
            else if (arg !== undefined && arg.toString !== undefined) {
                if (Array.isArray(arg) && arg.length === 3) {
                    this.node = arg[0];
                    this.id_a = arg[1];
                    this.id_b = arg[2];
                }
                else if (arg.node && arg.id_a && arg.id_b) {
                    this.node = arg.node;
                    this.id_a = arg.id_a;
                    this.id_b = arg.id_b;
                }
                else {
                    this.node = arg.toString();
                    this.id_a = arg.toString();
                    this.id_b = arg.toString();
                }
            }
            else {
                this.node = 0;
                this.id_a = Pid[rng](0, 32767); // Max is 15 bits
                this.id_b = Pid[rng](0, 7);     // Max is 3 bits
            }
        }

        getValue() {
            return [this.node, this.id_a, this.id_b];
        }

        getResult(noConvertToJs) {
            const procValue = Process.getProcess(this).value;
            if (noConvertToJs) {
                return procValue;
            }
            else {
                return jrts.erlangToJs(procValue);
            }
        }

        toString() {
            return `<${this.node}.${this.id_a}.${this.id_b}>`;
        }

        match(x) {
            if (Pid.isPid(x) && this.getValue().toString() === x.getValue().toString()) {
                return x;
            }
            else {
                return undefined;
            }
        }

        static [rng](min, max) {
            min = Math.ceil(min);
            max = Math.floor(max);
            return Math.floor(Math.random() * (max - min)) + min;
        }

        static isPid(m) {
            return m instanceof Pid;
        }
    }
})();