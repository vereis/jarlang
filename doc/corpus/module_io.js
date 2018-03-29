const io = function () {
    'use_strict';
    const exports = {

        'format': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['format/1'].bind(this)(...args);
                    } else {
                        return jrts.spawn(function() {
                            return functions['format/1'].bind(this)(...args);
                        });
                    }
                    break;
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['format/2'].bind(this)(...args);
                    } else {
                        return jrts.spawn(function() {
                            return functions['format/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function' + ('format' + ('/' + arguments.length));
        },

        'module_info': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 0:
                    if (Process.isProcess(this)) {
                        return functions['module_info/0'].bind(this)(...args);
                    } else {
                        return jrts.spawn(function() {
                            return functions['module_info/0'].bind(this)(...args);
                        });
                    }
                    break;
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['module_info/1'].bind(this)(...args);
                    } else {
                        return jrts.spawn(function() {
                            return functions['module_info/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function' + ('module_info' + ('/' + arguments.length));
        }
    };

    const functions = {
        'columns/0': function () {
            var cols = 1;

            if (typeof process == "object" && process.stdout) {
                cols = process.stdout.columns;
            }

            return new Tuple(new Atom("ok"), new Int(cols));
        },
        'columns/1': function (_cor0) {
            return this['columns/0']();
        },

        'format/1': function (_cor0) {
            console.log(_cor0.toString());
            return new Atom("ok");
        },
        'format/2': function (_cor1, _cor0) {
            console.log(formatErlangString(_cor1, _cor0).toString());
            return new Atom("ok");
        },
        'format/3': function (_cor2, _cor1, _cor0) {
            this['format/2'](_cor1, _cor0);
        },

        'fread/2': function (_cor1, _cor0) {
        },
        'fread/3': function (_cor2, _cor1, _cor0) {
            return this['fread/2'](_cor1, _cor0);
        },

        'fwrite/1': function (_cor0) {
            return this['format/1'](_cor0);
        },
        'fwrite/2': function (_cor1, _cor0) {
            return this['format/2'](_cor1, _cor0);
        },
        'fwrite/3': function (_cor2, _cor1, _cor0) {
            return this['format/2'](_cor1, _cor0);
        },

        'get_chars/2': function (_cor1, _cor0) {
        },
        'get_chars/3': function (_cor2, _cor1, _cor0) {
            return this['get_chars/2'](_cor1, _cor0);
        },

        'get_line/1': function (_cor0) {
        },
        'get_line/2': function (_cor1, _cor0) {
            return this['get_line/1'](_cor0);
        },

        'getopts/0': function () {
        },
        'getopts/1': function (_cor0) {
            return this['getopts/0']();
        },

        'module_info/0': function () {
        },
        'module_info/1': function (_cor0) {
        },

        'nl/0': function () {
            console.log("\n");
            return new Atom("ok");
        },
        'nl/1': function (_cor0) {
            return this['nl/0']();
        },

        'parse_erl_exprs/1': function (_cor0) {
        },
        'parse_erl_exprs/2': function (_cor1, _cor0) {
        },
        'parse_erl_exprs/3': function (_cor2, _cor1, _cor0) {
        },
        'parse_erl_exprs/4': function (_cor3, _cor2, _cor1, _cor0) {
        },

        'parse_erl_form/1': function (_cor0) {
        },
        'parse_erl_form/2': function (_cor1, _cor0) {
        },
        'parse_erl_form/3': function (_cor2, _cor1, _cor0) {
        },
        'parse_erl_form/4': function (_cor3, _cor2, _cor1, _cor0) {
        },

        'printable_range/0': function () {
        },

        'put_chars/1': function (_cor0) {
        },
        'put_chars/2': function (_cor1, _cor0) {
            return this['put_chars/1'](_cor0);
        },

        'read/1': function (_cor0) {
        },
        'read/2': function (_cor1, _cor0) {
        },
        'read/3': function (_cor2, _cor1, _cor0) {
        },
        'read/4': function (_cor3, _cor2, _cor1, cor0) {
        },

        'rows/0': function () {
            var rows = 1;

            if (typeof process == "object" && process.stdout) {
                rows = process.stdout.rows;
            }

            return new Tuple(new Atom("ok"), new Int(rows));
        },
        'rows/1': function (_cor0) {
            return this['rows/0']();
        },

        'scan_erl_exprs/1': function (_cor0) {
        },
        'scan_erl_exprs/2': function (_cor1, _cor0) {
        },
        'scan_erl_exprs/3': function (_cor2, _cor1, _cor0) {
        },
        'scan_erl_exprs/4': function (_cor3, _cor2, _cor1, _cor0) {
        },

        'scan_erl_form/1': function (_cor0) {
        },
        'scan_erl_form/2': function (_cor1, _cor0) {
        },
        'scan_erl_form/3': function (_cor2, _cor1, _cor0) {
        },
        'scan_erl_form/4': function (_cor3, _cor2, _cor1, _cor0) {
        },

        'setopts/1': function (_cor0) {
        },
        'setopts/2': function (_cor1, _cor0) {
            return this['setopts/1'](_cor0);
        },

        'write/1': function (_cor0) {
        },
        'write/2': function (_cor1, _cor0) {
            return this['write/1'](_cor0);
        }
    };

    // Private Methods
    function formatErlangString(str, data) {
        var isErlList = str instanceof List, fstr = "";
        str = isErlList ? str.toString() : str;
        data = isErlList ? [...data] : data;

        for (var i = 0; i < str.length; i++) {
            var ch = str.charAt(i);
            var d = null;
            if (ch == "~") {
                i++;
                let cur = str.charAt(i);
                switch (cur) {
                    case "~":
                        fstr += "~";
                        break;


                    case "c":
                        fstr += String.fromCharCode(data.shift());
                        break;


                    case "g":
                    case "f":
                        d = data.shift();
                        var isIntOrFloat = (Int.isInt(d) || Float.isFloat(d));
                        if (isIntOrFloat) {
                            var dval = d.getValue().toNumber();
                            var flagTestF  = (cur === "f");
                            var rangeTestG = (dval >= 0.1 && dval < 10000);
                            var flagTestG  = (cur === "g" && rangeTestG);
                            if (flagTestF || flagTestG) {
                                fstr += dval.toFixed(6);
                                break;
                            }
                            else {
                                data.unshift(d);
                                // fall through to case e
                            }
                        }
                        else {
                            throw `** format error: type ${d.constructor.name} cannot be coerced into type Float ("~f", [${d}])`;
                            break;
                        }
                    case "e":
                        d = data.shift();
                        var dval = d.getValue().toNumber();
                        if (Int.isInt(d) || Float.isFloat(d) || (cur === "g")) {
                            fstr += dval.toExponential(6);
                        }
                        else {
                            throw `** format error: type ${d.constructor.name} cannot be coerced into type Float ("~f", [${d}])`;
                        }
                        break;


                    case "s":
                        d = data.shift();
                        if (List.isList(d) || BitString.isBitString(d)) {
                            fstr += [...d].map((c) => {
                                return String.fromCharCode(c.getValue());
                            }).join("");
                        }
                        else if (Atom.isAtom(d)) {
                            fstr += d.getValue();
                        }
                        else {
                            throw `** format error: type ${d.constructor.name} cannot be coerced into type String ("~s", [${d}])`;
                        }
                        break;


                    case "W":
                    case "w":
                    case "P":
                    case "p":
                        fstr += data.shift().toString();
                        break;

                    case "#":
                    case "+":
                    case "b":
                    case "x":
                    case "X":
                    case "B":
                        d = data.shift();
                        if (Int.isInt(d) || Float.isFloat(d)) {
                            var dval = d.getValue().toNumber();
                            fstr += "#" + Number(dval).toString(16);
                        }
                        else {
                            throw `** format error: type ${d.constructor.name} cannot be coerced into type Base N Number ("~f", [${d}])`;
                        }
                        break;

                    case "n":
                        fstr += "\n";
                        break;


                    case "i":
                        data.shift();
                        break;


                    default:
                        throw "** bad argument: unknown flag ~" + cur;
                }
            }
            else {
                fstr += ch;
            }
        }

        //console.log("raw: ", fstr.split(""));
        return isErlList ? jrts.jsToErlang(fstr) : fstr;
    }

    return exports;
}();
