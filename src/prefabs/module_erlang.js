const erlang = function () {
    'use_strict';
    const exports = {
        '+': function () {
            switch (arguments.length) {
                case 2:
                    return functions['+/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('+' + ('/' + arguments.length));
        },
        '-': function () {
            switch (arguments.length) {
                case 2:
                    return functions['-/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('-' + ('/' + arguments.length));
        },
        '*': function () {
            switch (arguments.length) {
                case 2:
                    return functions['*/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('*' + ('/' + arguments.length));
        },
        '/': function () {
            switch (arguments.length) {
                case 2:
                    return functions['//2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('/' + ('/' + arguments.length));
        },
        'rem': function () {
            switch (arguments.length) {
                case 2:
                    return functions['rem/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('rem' + ('/' + arguments.length));
        },
        'div': function () {
            switch (arguments.length) {
                case 2:
                    return functions['div/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('div' + ('/' + arguments.length));
        },
        '==': function () {
            switch (arguments.length) {
                case 2:
                    return functions['==/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('==' + ('/' + arguments.length));
        },
        '/=': function () {
            switch (arguments.length) {
                case 2:
                    return functions['/=/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('/=' + ('/' + arguments.length));
        },
        '<': function () {
            switch (arguments.length) {
                case 2:
                    return functions['</2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('<' + ('/' + arguments.length));
        },
        '=<': function () {
            switch (arguments.length) {
                case 2:
                    return functions['=</2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('=<' + ('/' + arguments.length));
        },
        '>': function () {
            switch (arguments.length) {
                case 2:
                    return functions['>/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('>' + ('/' + arguments.length));
        },
        '>=': function () {
            switch (arguments.length) {
                case 2:
                    return functions['>=/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('>=' + ('/' + arguments.length));
        },
        '=:=': function () {
            switch (arguments.length) {
                case 2:
                    return functions['=:=/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('=:=' + ('/' + arguments.length));
        },
        '=/=': function () {
            switch (arguments.length) {
                case 2:
                    return functions['=/=/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('=/=' + ('/' + arguments.length));
        },
        'compare': function () {
            switch (arguments.length) {
                case 2:
                    return functions['compare/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('compare' + ('/' + arguments.length));
        },
        'match': function () {
            switch (arguments.length) {
                case 2:
                    return functions['match/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('match' + ('/' + arguments.length));
        },
        'or': function () {
            switch (arguments.length) {
                case 2:
                    return functions['or/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('or' + ('/' + arguments.length));
        },
        'and': function () {
            switch (arguments.length) {
                case 2:
                    return functions['and/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('and' + ('/' + arguments.length));
        },
        'not': function () {
            switch (arguments.length) {
                case 1:
                    return functions['not/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('not' + ('/' + arguments.length));
        },
        'xor': function () {
            switch (arguments.length) {
                case 2:
                    return functions['xor/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('xor' + ('/' + arguments.length));
        },
        'band': function () {
            switch (arguments.length) {
                case 2:
                    return functions['band/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('band' + ('/' + arguments.length));
        },
        'bor': function () {
            switch (arguments.length) {
                case 2:
                    return functions['bor/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('bor' + ('/' + arguments.length));
        },
        'bxor': function () {
            switch (arguments.length) {
                case 2:
                    return functions['bxor/2'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('bxor' + ('/' + arguments.length));
        },
        'bnot': function () {
            switch (arguments.length) {
                case 1:
                    return functions['bnot/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('bnot' + ('/' + arguments.length));
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
        '+/2': function (_cor1, _cor0) {
            if (ErlNumber.isErlNumber(_cor1) && ErlNumber.isErlNumber(_cor0)) {
                return _cor1.add(_cor0);
            }
            throw '** exception error: an error occurred when evaluating an arithmetic expression in operator +/2 called as ' + _cor1 + ' + ' + _cor0;
        },
        '-/2': function (_cor1, _cor0) {
            if (ErlNumber.isErlNumber(_cor1) && ErlNumber.isErlNumber(_cor0)) {
                return _cor1.subtract(_cor0);
            }
            throw '** exception error: an error occurred when evaluating an arithmetic expression in operator -/2 called as ' + _cor1 + ' - ' + _cor0;
        },
        '*/2': function (_cor1, _cor0) {
            if (ErlNumber.isErlNumber(_cor1) && ErlNumber.isErlNumber(_cor0)) {
                return _cor1.multiply(_cor0);
            }
            throw '** exception error: an error occurred when evaluating an arithmetic expression in operator */2 called as ' + _cor1 + ' * ' + _cor0;
        },
        'division/2': function (_cor1, _cor0) {
            if (ErlNumber.isErlNumber(_cor1) && ErlNumber.isErlNumber(_cor0)) {
                return _cor1.divide(_cor0);
            }
            throw '** exception error: an error occurred when evaluating an arithmetic expression in operator \'/\'/2 called as ' + _cor1 + ' / ' + _cor0;
        },
        'rem/2': function (_cor1, _cor0) {
            if (ErlNumber.isErlNumber(_cor1) && ErlNumber.isErlNumber(_cor0)) {
                return _cor1.remainder(_cor0);
            }
            throw '** exception error: an error occurred when evaluating an arithmetic expression in operator rem/2 called as ' + _cor1 + ' rem ' + _cor0;
        },
        'div/2': function (_cor1, _cor0) {
            if (ErlNumber.isErlNumber(_cor1) && ErlNumber.isErlNumber(_cor0)) {
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
            ;
        },
        'module_info/1': function () {
            ;
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
            return ErlNumber.isErlNumber(_cor0) && _cor0.isFloat();
        },
        'is_function/1': function (_cor0) {
            return Fun.isFun(_cor0);
        },
        'is_integer/1': function (_cor0) {
            return ErlNumber.isErlNumber(_cor0) && _cor0.isInteger();
        },
        'is_list/1': function (_cor0) {
            return List.isList(_cor0);
        },
        'is_map/1': function (_cor0) {
            return ErlMap.isErlMap(_cor0);
        },
        'is_number/1': function (_cor0) {
            return ErlNumber.isErlNumber(_cor0);
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
            return new ErlNumber(_cor0.getValue().abs());
        },
        'ceil/1': function (_cor0) {
            return new ErlNumber(_cor0.getValue().ceil());
        },
        'floor/1': function (_cor0) {
            return new ErlNumber(_cor0.getValue().floor());
        },
        'trunc/1': function (_cor0) {
            return new ErlNumber(_cor0.getValue().trunc());
        }
    };

    // Private Methods -----------------------------------------------------------------------------------------------

    //returns true or false, contrary to erlang matching behavior
    function match(_cor1, _cor0) {
        if (_cor1 == undefined) {
            return true;
        }
        else if (_cor1 instanceof ErlNumber) {
            return _cor0 instanceof ErlNumber && _cor1.equals(_cor0);
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
        if (_cor1 instanceof ErlNumber) {
            if (_cor0 instanceof ErlNumber) {
                return _cor1.subtract(_cor0);
            }
            
            return -1;
        }
        else if (_cor1 instanceof Atom) {
            if (_cor0 instanceof ErlNumber) {
                return 1;
            }
            else if (_cor0 instanceof Atom) {
                if (_cor1.value < _cor0.value) {
                    return -1;
                }
                else if (_cor1.value > _cor0.value) {
                    return 1;
                }
                else {
                    return 0;
                }
            }
            
            return -1;
        }
        else if (_cor1 instanceof Reference) {
            if (_cor0 instanceof ErlNumber || _cor0 instanceof Atom) {
                return 1;
            }
            else if (_cor0 instanceof Reference) {
                if (_cor1.value < _cor0.value) {
                    return -1;
                }
                else if (_cor1.value > _cor0.value) {
                    return 1;
                }
                else {
                    return 0;
                }
            }
            
            return -1;
        }
        else if (_cor1 instanceof Fun) {
            if (_cor0 instanceof ErlNumber || 
                _cor0 instanceof Atom || 
                _cor0 instanceof Reference) {
                    return 1;
            }
            else if (_cor0 instanceof Fun) {
                throw "Fun is not implemented yet";
            }
            
            return -1;
        }
        else if (_cor1 instanceof Port) {
            if (_cor0 instanceof ErlNumber || 
                _cor0 instanceof Atom || 
                _cor0 instanceof Reference || 
                _cor0 instanceof Fun) {
                    return 1;
            }
            else if (_cor0 instanceof Port) {
                throw "Port is not implemented yet";
            }
            
            return -1;
        }
        else if (_cor1 instanceof Pid) {
            if (_cor0 instanceof ErlNumber || 
                _cor0 instanceof Atom || 
                _cor0 instanceof Reference || 
                _cor0 instanceof Fun || 
                _cor0 instanceof Port) {
                    return 1;
            }
            else if (_cor0 instanceof Pid) {
                throw "Pid is not implemented yet";
            }
            
            return -1;
        }
        else if (_cor1 instanceof Tuple) {
            if (_cor0 instanceof ErlNumber || 
                _cor0 instanceof Atom || 
                _cor0 instanceof Reference || 
                _cor0 instanceof Fun || 
                _cor0 instanceof Port || 
                _cor0 instanceof Pid) {
                    return 1;
            }
            else if (_cor0 instanceof Tuple) {
                if (_cor1.size() < _cor0.size()) {
                    return -1;
                }
                else if (_cor1.size() > _cor0.size()) {
                    return 1;
                }
                else {
                    let len = Math.min(_cor1.size(), _cor0.size());
                    for (let i = 0; i < len; i++) {
                        let c = compare(_cor1.nth(i), _cor0.nth(i));
                        if (c == 0) {
                            continue;
                        }
                        else {
                            return c;
                        }
                    }

                    return 0;
                }
            }
            
            return -1;
        }
        else if (_cor1 instanceof Map) {
            if (_cor0 instanceof ErlNumber || 
                _cor0 instanceof Atom || 
                _cor0 instanceof Reference || 
                _cor0 instanceof Fun || 
                _cor0 instanceof Port || 
                _cor0 instanceof Pid || 
                _cor0 instanceof Tuple) {
                    return 1;
            }
            else if (_cor0 instanceof Map) {
                throw "Map is not implemented yet";
            }
            
            return -1;
        }
        else if (_cor1 instanceof List) {
            if (_cor0 instanceof ErlNumber || 
                _cor0 instanceof Atom || 
                _cor0 instanceof Reference || 
                _cor0 instanceof Fun || 
                _cor0 instanceof Port || 
                _cor0 instanceof Pid || 
                _cor0 instanceof Tuple || 
                _cor0 instanceof Map) {
                    return 1;
            }
            else if (_cor0 instanceof List) {
                if (List.isEmptyList(_cor1)) {
                    if (!List.isEmptyList(_cor0)) {
                        return -1;
                    }
                    
                    return 0;
                } 
                else {
                    if (List.isEmptyList(_cor0)) {
                        return 1;
                    }
                    else {
                        let len = Math.min(_cor1.size(), _cor0.size());
                        for (let i = 0; i < len; i++) {
                            let c = compare(_cor1.nth(i), _cor0.nth(i));
                            if (c == 0) {
                                continue;
                            }
                            else {
                                return c;
                            }
                        }
                        //if one list is the sublist of the other then the shortest list goes first
                        if (_cor1.size() < _cor0.size()) {
                            return -1;
                        }
                        else if (_cor1.size() > _cor0.size()) {
                            return 1;
                        }
                        
                        return 0;
                    }
                }
            }
            return -1;
        }
        else if (_cor1 instanceof BitString) {
            if (_cor0 instanceof ErlNumber || 
                _cor0 instanceof Atom || 
                _cor0 instanceof Reference || 
                _cor0 instanceof Fun || 
                _cor0 instanceof Port || 
                _cor0 instanceof Pid || 
                _cor0 instanceof Tuple || 
                _cor0 instanceof Map || 
                _cor0 instanceof List) {
                    return 1;
            }
            else if (_cor0 instanceof BitString) {
                throw "BitString is not implemented yet";
            }
            
            return -1;
        }
        
        //If _cor1 is not an Erlang datatype wrapper then fail over to js comparisons and hope for the best
        else {
             if (_cor1 < _cor0) return -1;
            else if (_cor1 > _cor0) return 1;
            else return 0;
        }
    }

    return exports;
}();
