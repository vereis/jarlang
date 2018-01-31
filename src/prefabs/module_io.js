const io = function () {
    'use_strict';
    const exports = {
        
        'format': function () {
            switch (arguments.length) {
            case 1:
                return functions['format/1'](...arguments);
                break;
            case 2:
                return functions['format/2'](...arguments);
                break;
            }
            throw '** exception error: undefined function' + ('format' + ('/' + arguments.length));
        },
        
        
        'module_info': function () {
            switch (arguments.length) {
            case 0:
                return functions['module_info/0'](...arguments);
                break;
            case 1:
                return functions['module_info/1'](...arguments);
                break;
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

            return new Tuple(new Atom("ok"), new ErlNumber(cols));
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

            return new Tuple(new Atom("ok"), new ErlNumber(rows));
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

        for (int i = 0; i < str.length; i++) {
            var ch = str.charAt(i);

            if (ch == "~") {
                i++;

                switch (str.charAt(i)) {
                    case "~":
                        fstr += "~";
                        break;
                    case "c":
                        fstr += String.fromCharCode(data.shift());
                        break;
                    case "f":
                        break;
                    case "e":
                        break;
                    case "g":
                        break;
                    case "s":
                        fstr += data.shift();
                        break;
                    case "w":
                        break;
                    case "p":
                        break;
                    case "W":
                        break;
                    case "P":
                        break;
                    case "B":
                        break;
                    case "X":
                        break;
                    case "#":
                        break;
                    case "b":
                        break;
                    case "x":
                        break;
                    case "+":
                        break;
                    case "n":
                        fstr += "\n";
                        break;
                    case "i":
                        data.shift();
                        break;
                    default:
                        throw "bad argument";
                }
            }
            else {
                fstr += ch;
            }
        }

        return isErlList ? new List(...fstr.split("")) : fstr;
    }

    return exports;
}();
