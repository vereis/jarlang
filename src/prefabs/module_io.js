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
            if (typeof process == "object") {
                return new Tuple(new Atom("ok"), new ErlNumber(process.stdout.columns));
            }
        },
        'columns/1': function (_cor0) {
            this['columns/0']();
        },
        
        'format/1': function (_cor0) {
            console.log(_cor0);
            return 'ok';
        },
        'format/2': function (_cor1, _cor0) {
            console.log(_cor1);
            return 'ok';
        },
        'format/3': function (_cor2, _cor1, _cor0) {
            this['format/2'](_cor1, _cor0);
        },

        'fread/2': function (_cor1, _cor0) {
        },
        'fread/3': function (_cor2, _cor1, _cor0) {
        },

        'fwrite/1': function (_cor0) {
        },
        'fwrite/2': function (_cor1, _cor0) {
        },
        'fwrite/3': function (_cor2, _cor1, _cor0) {
        },

        'get_chars/2': function (_cor1, _cor0) {
        },
        'get_chars/3': function (_cor2, _cor1, _cor0) {
        },

        'get_line/1': function (_cor0) {
        },
        'get_line/2': function (_cor1, _cor0) {
        },

        'getopts/0': function () {
        },
        'getopts/1': function (_cor0) {
        },

        'module_info/0': function () {
        },
        'module_info/1': function (_cor0) {
        },

        'nl/0': function () {
        },
        'nl/1': function (_cor0) {
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
            if (typeof process == "object") {
                return new Tuple(new Atom("ok"), new ErlNumber(process.stdout.rows));
            }
        },
        'rows/1': function (_cor0) {
            this['rows/0']();
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
        },

        'write/1': function (_cor0) {
        },
        'write/2': function (_cor1, _cor0) {
        }
    };
    return exports;
}();
