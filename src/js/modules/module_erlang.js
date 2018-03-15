const erlang = function () {
    'use_strict';
    const exports = {
        '!': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['!/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['!/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('!' + ('/' + arguments.length));
        },
        '+': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['+/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['+/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('+' + ('/' + arguments.length));
        },
        '-': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['-/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['-/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('-' + ('/' + arguments.length));
        },
        '*': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['*/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['*/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('*' + ('/' + arguments.length));
        },
        '/': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['//2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['//2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('/' + ('/' + arguments.length));
        },
        'rem': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['rem/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['rem/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('rem' + ('/' + arguments.length));
        },
        'div': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['div/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['div/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('div' + ('/' + arguments.length));
        },
        '==': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['==/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['==/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('==' + ('/' + arguments.length));
        },
        '/=': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['/=/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['/=/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('/=' + ('/' + arguments.length));
        },
        '<': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['</2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['</2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('<' + ('/' + arguments.length));
        },
        '=<': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['=</2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['=</2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('=<' + ('/' + arguments.length));
        },
        '>': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['>/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['>/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('>' + ('/' + arguments.length));
        },
        '>=': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['>=/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['>=/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('>=' + ('/' + arguments.length));
        },
        '=:=': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['=:=/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['=:=/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('=:=' + ('/' + arguments.length));
        },
        '=/=': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['=/=/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['=/=/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('=/=' + ('/' + arguments.length));
        },
        'compare': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['compare/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['compare/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('compare' + ('/' + arguments.length));
        },
        'match': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['match/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['match/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('match' + ('/' + arguments.length));
        },
        'or': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['or/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['or/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('or' + ('/' + arguments.length));
        },
        'and': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['and/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['and/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('and' + ('/' + arguments.length));
        },
        'not': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['not/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['not/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('not' + ('/' + arguments.length));
        },
        'xor': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['xor/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['xor/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('xor' + ('/' + arguments.length));
        },
        'band': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['band/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['band/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('band' + ('/' + arguments.length));
        },
        'bor': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['bor/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['bor/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('bor' + ('/' + arguments.length));
        },
        'bxor': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['bxor/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['bxor/2'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('bxor' + ('/' + arguments.length));
        },
        'bnot': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['bnot/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['bnot/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('bnot' + ('/' + arguments.length));
        },
        'module_info': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 0:
                    if (Process.isProcess(this)) {
                        return functions['module_info/0'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['module_info/0'].bind(this)(...args);
                        });
                    }
                    break;
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['module_info/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['module_info/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('module_info' + ('/' + arguments.length));
        },
        'atom_to_list': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['atom_to_list/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['atom_to_list/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('atom_to_list' + ('/' + arguments.length));
        },
        'list_to_atom': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['list_to_atom/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['list_to_atom/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('list_to_atom' + ('/' + arguments.length));
        },
        'list_to_existing_atom': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['list_to_existing_atom/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['list_to_existing_atom/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('list_to_existing_atom' + ('/' + arguments.length));
        },
        'is_atom': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['is_atom/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['is_atom/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('is_atom' + ('/' + arguments.length));
        },
        'is_binary': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['is_binary/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['is_binary/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('is_binary' + ('/' + arguments.length));
        },
        'is_bitstring': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['is_bitstring/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['is_bitstring/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('is_bitstring' + ('/' + arguments.length));
        },
        'is_boolean': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['is_boolean/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['is_boolean/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('is_boolean' + ('/' + arguments.length));
        },
        'is_float': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['is_float/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['is_float/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('is_float' + ('/' + arguments.length));
        },
        'is_function': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['is_function/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['is_function/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('is_function ' + ('/' + arguments.length));
        },
        'is_integer': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['is_integer/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['is_integer/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('is_integer' + ('/' + arguments.length));
        },
        'is_list': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['is_list/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['is_list/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('is_list' + ('/' + arguments.length));
        },
        'is_map': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['is_map/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['is_map/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('is_map' + ('/' + arguments.length));
        },
        'is_number': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['is_number/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['is_number/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('is_number' + ('/' + arguments.length));
        },
        'is_pid': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['is_pid/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['is_pid/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('is_pid' + ('/' + arguments.length));
        },
        'is_port': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['is_port/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['is_port/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('is_port' + ('/' + arguments.length));
        },
        'is_reference': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['is_reference/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['is_reference/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('is_reference' + ('/' + arguments.length));
        },
        'is_tuple': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['is_tuple/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['is_tuple/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('is_tuple' + ('/' + arguments.length));
        },
        'abs': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['abs/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['abs/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('abs' + ('/' + arguments.length));
        },
        'ceil': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['ceil/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['ceil/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('ceil' + ('/' + arguments.length));
        },
        'floor': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['floor/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['floor/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('floor' + ('/' + arguments.length));
        },
        'trunc': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['trunc/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['trunc/1'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('trunc' + ('/' + arguments.length));
        },
        'self': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 0:
                    if (Process.isProcess(this)) {
                        return functions['self/0'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['self/0'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('self' + ('/' + arguments.length));
        },
        'spawn': function () {
            let args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
                case 1:
                    if (Process.isProcess(this)) {
                        return functions['spawn/1'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['spawn/1'].bind(this)(...args);
                        });
                    }
                    break;
                case 2:
                    if (Process.isProcess(this)) {
                        return functions['spawn/2'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['spawn/2'].bind(this)(...args);
                        });
                    }
                    break;
                case 3:
                    if (Process.isProcess(this)) {
                        return functions['spawn/3'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['spawn/3'].bind(this)(...args);
                        });
                    }
                    break;
                case 4:
                    if (Process.isProcess(this)) {
                        return functions['spawn/4'].bind(this)(...args);
                    }
                    else {
                        return jrts.spawn(function () {
                            return functions['spawn/4'].bind(this)(...args);
                        });
                    }
            }
            throw '** exception error: undefined function ' + ('spawn' + ('/' + arguments.length));
        }
    };

    const functions = {
        '!/2': function (_cor1, _cor0) {
            if (Pid.isPid(_cor1)) {
                return _cor1.sendMessage(_cor0);
            }
            throw '** exception error: bad argument in operator !/2 called as ' + _cor1 + ' ! ' + _cor0;
        },
        '+/2': function (_cor1, _cor0) {
            if ((Int.isInt(_cor1) || Float.isFloat(_cor1)) && (Int.isInt(_cor0) || Float.isFloat(_cor0))) {
                return _cor1.add(_cor0);
            }
            throw '** exception error: an error occurred when evaluating an arithmetic expression in operator +/2 called as ' + _cor1 + ' + ' + _cor0;
        },
        '-/2': function (_cor1, _cor0) {
            if ((Int.isInt(_cor1) || Float.isFloat(_cor1)) && (Int.isInt(_cor0) || Float.isFloat(_cor0))) {
                return _cor1.subtract(_cor0);
            }
            throw '** exception error: an error occurred when evaluating an arithmetic expression in operator -/2 called as ' + _cor1 + ' - ' + _cor0;
        },
        '*/2': function (_cor1, _cor0) {
            if ((Int.isInt(_cor1) || Float.isFloat(_cor1)) && (Int.isInt(_cor0) || Float.isFloat(_cor0))) {
                return _cor1.multiply(_cor0);
            }
            throw '** exception error: an error occurred when evaluating an arithmetic expression in operator */2 called as ' + _cor1 + ' * ' + _cor0;
        },
        'division/2': function (_cor1, _cor0) {
            if ((Int.isInt(_cor1) || Float.isFloat(_cor1)) && (Int.isInt(_cor0) || Float.isFloat(_cor0))) {
                return _cor1.divide(_cor0);
            }
            throw '** exception error: an error occurred when evaluating an arithmetic expression in operator \'/\'/2 called as ' + _cor1 + ' / ' + _cor0;
        },
        'rem/2': function (_cor1, _cor0) {
            if (Int.isInt(_cor1) && Int.isInt(_cor0)) {
                return _cor1.remainder(_cor0);
            }
            throw '** exception error: an error occurred when evaluating an arithmetic expression in operator rem/2 called as ' + _cor1 + ' rem ' + _cor0;
        },
        'div/2': function (_cor1, _cor0) {
            if (Int.isInt(_cor1) && Int.isInt(_cor0)) {
                return _cor1.intDivide(_cor0);
            }
            throw '** exception error: an error occurred when evaluating an arithmetic expression in operator div/2 called as ' + _cor1 + ' div ' + _cor0;
        },
        '==/2': function (_cor1, _cor0) {
            return compare(_cor1, _cor0) == 0;
        },
        '/=/2': function (_cor1, _cor0) {
            return compare(_cor1, _cor0) != 0;
        },
        '</2': function (_cor1, _cor0) {
            return compare(_cor1, _cor0) < 0;
        },
        '=</2': function (_cor1, _cor0) {
            return compare(_cor1, _cor0) <= 0;
        },
        '>/2': function (_cor1, _cor0) {
            return compare(_cor1, _cor0) > 0;
        },
        '>=/2': function (_cor1, _cor0) {
            return compare(_cor1, _cor0) >= 0;
        },
        '=:=/2': function (_cor1, _cor0) {
            return _cor1 === _cor0;
        },
        '=/=/2': function (_cor1, _cor0) {
            return _cor1 !== _cor0;
        },
        //returns negative if _cor1 evaluates to less than _cor0, returns 0 if they evaluate equal. Magnitude is not representative of anything
        'compare/2': compare,
        //returns true or false, contrary to erlang matching behavior
        'match/2': match,
        'or/2': function (_cor1, _cor0) {
            return _cor1 || _cor0;
        },
        'and/2': function (_cor1, _cor0) {
            return _cor1 && _cor0;
        },
        'not/1': function (_cor0) {
            return !_cor0;
        },
        'xor/2': function (_cor1, _cor0) {
            return _cor1 && !_cor0 || _cor0 && !_cor1;
        },
        'bor/2': function (_cor1, _cor0) {
            return _cor1 | _cor0;
        },
        'band/2': function (_cor1, _cor0) {
            return _cor1 & _cor0;
        },
        'bnot/1': function (_cor0) {
            return ~_cor0;
        },
        'bxor/2': function (_cor1, _cor0) {
            return _cor1 ^ _cor0;
        },
        'module_info/0': function () {
        },
        'module_info/1': function () {
        },

        'atom_to_binary/2': function (_cor1, _cor0) {
            return;
        },
        'atom_to_list/1': function (_cor0) {
            if (_cor0 instanceof Atom) {
                return new List(_cor0.toString());
            }
            throw "bad argument";
        },
        'binary_to_atom/2': function (_cor1, _cor0) {
            return;
        },
        'binary_to_existing_atom/2': function (_cor1, _cor0) {
            return;
        },
        'binary_to_float/1': function (_cor0) {
            return;
        },
        'binary_to_integer/1': function (_cor0) {
            return;
        },
        'binary_to_integer/2': function (_cor1, _cor0) {
            return;
        },
        'binary_to_list/1': function (_cor0) {
            return;
        },
        'binary_to_list/3': function (_cor2, _cor1, _cor0) {
            return;
        },
        'binary_to_term/1': function (_cor0) {
            return;
        },
        'binary_to_term/2': function (_cor1, _cor0) {
            return;
        },
        'bitstring_to_list/1': function (_cor0) {
            return;
        },
        'float_to_binary/1': function (_cor0) {
            return;
        },
        'float_to_binary/2': function (_cor1, _cor0) {
            return;
        },
        'float_to_list/1': function (_cor0) {
            return;
        },
        'float_to_list/2': function (_cor1, _cor0) {
            return;
        },
        'fun_to_list/1': function (_cor0) {
            return;
        },
        'integer_to_binary/1': function (_cor0) {
            return;
        },
        'integer_to_binary/2': function (_cor1, _cor0) {
            return;
        },
        'integer_to_list/1': function (_cor0) {
            return;
        },
        'integer_to_list/2': function (_cor1, _cor0) {
            return;
        },
        'iolist_to_binary/1': function (_cor0) {
            return;
        },
        'iolist_to_iovec/1': function (_cor0) {
            return;
        },
        'list_to_atom/1': function (_cor0) {
            if (_cor0 instanceof List && List.isString(_cor0)) {
                return new Atom(_cor0.toString);
            }
            throw "bad argument";
        },
        'list_to_binary/1': function (_cor0) {
            return;
        },
        'list_to_bitstring/1': function (_cor0) {
            return;
        },
        'list_to_existing_atom/1': function (_cor0) {
            if (_cor0 instanceof List && List.isString(_cor0) && Atom.exists(_cor0.toString())) {
                return new Atom(_cor0.toString);
            }
            throw "bad argument";
        },
        'list_to_float/1': function (_cor0) {
            return;
        },
        'list_to_integer/1': function (_cor0) {
            return;
        },
        'list_to_integer/2': function (_cor1, _cor0) {
            return;
        },
        'list_to_pid/1': function (_cor0) {
            return;
        },
        'list_to_port/1': function (_cor0) {
            return;
        },
        'list_to_ref/1': function (_cor0) {
            return;
        },
        'list_to_tuple/1': function (_cor0) {
            return;
        },
        'localtime_to_universaltime/1': function (_cor0) {
            return;
        },
        'localtime_to_universaltime/2': function (_cor1, _cor0) {
            return;
        },
        'pid_to_list/1': function (_cor0) {
            return;
        },
        'port_to_list/1': function (_cor0) {
            return;
        },
        'ref_to_list/1': function (_cor0) {
            return;
        },
        'term_to_binary/1': function (_cor0) {
            return;
        },
        'term_to_binary/2': function (_cor1, _cor0) {
            return;
        },
        'tuple_to_list/1': function (_cor0) {
            return;
        },
        'universaltime_to_localtime/1': function (_cor0) {
            return;
        },

        'is_atom/1': function (_cor0) {
            return Atom.isAtom(_cor0);
        },
        'is_binary/1': function (_cor0) {
            return BitString.isBinary(_cor0);
        },
        'is_bitstring/1': function (_cor0) {
            return BitString.isBitString(_cor0);
        },
        'is_boolean/1': function (_cor0) {
            return Atom.isAtom(_cor0) && (_cor0.toString() == "true" || _cor0.toString == "false");
        },
        'is_float/1': function (_cor0) {
            return Float.isFloat(_cor0);
        },
        'is_function/1': function (_cor0) {
            return Fun.isFun(_cor0);
        },
        'is_integer/1': function (_cor0) {
            return Int.isInt(_cor0);
        },
        'is_list/1': function (_cor0) {
            return List.isList(_cor0);
        },
        'is_map/1': function (_cor0) {
            return ErlMap.isErlMap(_cor0);
        },
        'is_number/1': function (_cor0) {
            return Int.isInt(_cor0) || Float.isFloat(_cor0);
        },
        'is_pid/1': function (_cor0) {
            return Pid.isPid(_cor0);
        },
        'is_port/1': function (_cor0) {
            return Port.isPort(_cor0);
        },
        'is_reference/1': function (_cor0) {
            return Reference.isReference(_cor0);
        },
        'is_tuple/1': function (_cor0) {
            return Tuple.isTuple(_cor0);
        },

        'abs/1': function (_cor0) {
            return Int.isInt(_cor0) ? new Int(_cor0.getValue().abs()) : new Float(_cor0.getValue().abs());
        },
        'ceil/1': function (_cor0) {
            return new Int(_cor0.getValue().ceil());
        },
        'floor/1': function (_cor0) {
            return new Int(_cor0.getValue().floor());
        },
        'trunc/1': function (_cor0) {
            return new Int(_cor0.getValue().trunc());
        },

        'self/0': function () {
            return this.getValue();
        },
        'spawn/1': function (_cor0) {
            return functions["spawn/2"](null, _cor0); 
        },
        'spawn/2': function (_cor0, _cor1) {
            console.warn("Currently Jarlang processes don't store what module they're currently calling from " +
                         "and thus, we cannot guarentee that automatically coercing the module without explicityly " +
                         "specifying it will work.\n Please use spawn/3 or spawn/4 if you can for now.");
            return functions["spawn/4"](_cor0, null, _cor1, new List()); 
        },
        'spawn/3': function (_cor0, _cor1, _cor2) {
            return functions["spawn/4"](null, _cor0, _cor1, _cor2); 
        },
        'spawn/4': function (_cor0, _cor1, _cor2, _cor3) {
            console.warn("Currently Jarlang doesn't support spawning on different nodes " + 
                         "so this value will be ignored and any new processes will be spawned on the current node.");

            const modulename = _cor1 ? _cor1.toString() : "erlang";
            /* jshint ignore:start */
            const module = eval(modulename);
            /* jshint ignore:end */
            
            const fnname = `${_cor2.toString()}${module === this ? "/" + _cor3.size() : ""}`;
            const fn = module[fnname];

            if (fn !== undefined) {
                return jrts.spawn(function() {
                    return fn.bind(this)(..._cor3);
                });
            }
            else {
                throw '** exception error: undefined function ' + modulename + "['" + fnname + "']";
            }
        }
    };

    // Private Methods -----------------------------------------------------------------------------------------------

    //returns true or false, contrary to erlang matching behavior
    function match(_cor1, _cor0) {
        if (_cor1 == undefined) {
            return true;
        }
        else if (_cor1 instanceof Int) {
            return (_cor0 instanceof Int || _cor0 instanceof Float) && _cor1.equals(_cor0);
        }
        else if (_cor1 instanceof Float) {
            return (_cor0 instanceof Int || _cor0 instanceof Float) && _cor1.equals(_cor0);
        }
        else if (_cor1 instanceof Atom) {
            return _cor0 instanceof Atom && _cor1.value == _cor0.value;
        }
        else if (_cor1 instanceof Reference) {
            throw "Reference is not implemented yet";
        }
        else if (_cor1 instanceof Fun) {
            throw "Fun is not implemented yet";
        }
        else if (_cor1 instanceof Port) {
            throw "Port is not implemented yet";
        }
        else if (_cor1 instanceof Pid) {
            throw "Pid is not implemented yet";
        }
        else if (_cor1 instanceof Tuple) {
            // Shortcircuit comparison because unequal lengths must mean inequality
            if (_cor1.size() != _cor0.size()) {
                return false;
            }
            else {
                let len = _cor1.size();
                for (let i = 0; i < len; i++) {
                    let c = match(_cor1.nth(i), _cor0.nth(i));
                    if (c) {
                        continue;
                    }
                    else {
                        return false;
                    }
                }
                return true;
            }
        }
        else if (_cor1 instanceof Map) {
            throw "Map is not implemented yet";
        }
        else if (_cor1 instanceof List) {
            throw "List is not implemented yet";
        }
        else if (_cor1 instanceof BitString) {
            throw "BitString is not implemented yet";
        }
        throw "Tried to match on unrecognized type: " + _cor1.toString();
    }

    //returns negative if _cor1 evaluates to less than _cor0, returns 0 if they evaluate equal. Magnitude is not representative of anything
    function compare(_cor1, _cor0) {
        if (ErlangDatatype.isErlangDatatype(_cor1) && ErlangDatatype.isErlangDatatype(_cor0)) {
            const isNumber_cor1 = Int.isInt(_cor1) || Float.isFloat(_cor1);
            const isNumber_cor0 = Int.isInt(_cor0) || Float.isFloat(_cor0);
            if (isNumber_cor1 && isNumber_cor0) {
                return _cor1.greaterThan(_cor0) ?  1 :
                       _cor1.lessThan(_cor0)    ? -1 :
                                                   0;
            }
            else {
                // Lists are compared element by element regardless of size
                if (List.isList(_cor1) && List.isList(_cor0)) {
                    return (() => {
                                const _arr1 = [..._cor1];
                                const _arr0 = [..._cor0];
                                const bound = Math.max(_arr1.length, _arr0.length);
                                let res = 0;
                                for (let i = 0; i < bound; i++) {
                                    res = compare(_arr1[i] || 0, _arr0[i] || 0);
                                    if (res !== 0) {
                                        break;
                                    }
                                }
 
                                return res;
                           })();
                }
                // Tuples are first compared by length, before element by element comparison is used
                else if (Tuple.isTuple(_cor1) && Tuple.isTuple(_cor0)) {
                    const _size1 = _cor1.size();
                    const _size0 = _cor0.size();
                    return _size1 > _size0 ?  1 :
                           _size1 < _size0 ? -1 :
                           (() => {
                               const _arr1 = [..._cor1];
                               const _arr0 = [..._cor0];
                               let res = 0;
                               for (let i = 0; i < _size1; i++) {
                                   res = compare(_arr1[i], _arr0[i]);
                                   if (res !== 0) {
                                       break;
                                   }
                               }

                               return res;
                           })();
                }
                // Maps need to be compared by size, then compared by keys (ascending) then by value
                else if (Map.isMap(_cor1) && Map.isMap(_cor0)) {
                    const _keys1 = Object.keys(_cor1.getValue());
                    const _keys0 = Object.keys(_cor0.getValue());
                    const _size1 = _keys1.length;
                    const _size0 = _keys0.length;
                    return _size1 > _size0 ?  1 :
                           _size1 < _size0 ? -1 :
                           (() => {
                               let res = 0;
                               for (let i = 0; i < _size1; i++) {
                                   res = compare(_keys1[i], _keys0[i]);
                                   if (res !== 0) {
                                       return res;
                                   }
                               }
                               for (i = 0; i < _size1; i++) {
                                   res = compare(_cor1[_keys1[i]], _cor0[_keys0[i]]);
                                   if (res !== 0) {
                                       break;
                                   }
                               }
                               return res;
                           })();
                }
                else {
                    const _comparator1 = _cor1.getComparator();
                    const _comparator0 = _cor0.getComparator();
                    return _comparator1 > _comparator0 ?  1 :
                           _comparator1 < _comparator0 ? -1 :
                                                          0;
                }
            }
        }
    }

    return exports;
}();
