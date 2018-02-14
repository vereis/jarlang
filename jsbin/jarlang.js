/**
 * Runtime environment for Jarlang
 * Provides all of the neccessary environment functions to allow Jarlang to work as intended
 * such as our process queue
 */
const __erlang = (function (secondsPerTick) {
    'use strict';

    // Return a shuffled copy of an array, this is used so that the processes queue
    // is mapped over randomly each tick
    function shuffle(input) {
        let array = [...input];

        let count = array.length;
        let randomnumber;
        let temp;
        while (count) {
            randomnumber = Math.random() * count-- | 0;
            temp = array[count];
            array[count] = array[randomnumber];
            array[randomnumber] = temp;
        }

        return array;
    }

    const exports = {
        processes: [],
        pids: {},
        timer: setInterval(function () {
            shuffle(exports.processes).map(process => {
                if (process.lambdas.length) {
                    process.currentLambda = process.lambdas.shift();
                    process.currentLambda();
                    process.currentLambda = null;
                }
            });
        }, secondsPerTick !== undefined ? secondsPerTick : 1),
        spawn: function (lambda, pidGenerator) {
            return new Process(lambda, pidGenerator).pid;
        }
    };

    return exports;
})();
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
        },
        'atom_to_list': function () {
            switch (arguments.length) {
                case 1:
                    return functions['atom_to_list/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('atom_to_list' + ('/' + arguments.length));
        },
        'list_to_atom': function () {
            switch (arguments.length) {
                case 1:
                    return functions['list_to_atom/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('list_to_atom' + ('/' + arguments.length));
        },
        'list_to_existing_atom': function () {
            switch (arguments.length) {
                case 1:
                    return functions['list_to_existing_atom/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('list_to_existing_atom' + ('/' + arguments.length));
        },
        'is_atom': function () {
            switch (arguments.length) {
                case 1:
                    return functions['is_atom/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('is_atom' + ('/' + arguments.length));
        },
        'is_binary': function () {
            switch (arguments.length) {
                case 1:
                    return functions['is_binary/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('is_binary' + ('/' + arguments.length));
        },
        'is_bitstring': function () {
            switch (arguments.length) {
                case 1:
                    return functions['is_bitstring/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('is_bitstring' + ('/' + arguments.length));
        },
        'is_boolean': function () {
            switch (arguments.length) {
                case 1:
                    return functions['is_boolean/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('is_boolean' + ('/' + arguments.length));
        },
        'is_float': function () {
            switch (arguments.length) {
                case 1:
                    return functions['is_float/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('is_float' + ('/' + arguments.length));
        },
        'is_function': function () {
            switch (arguments.length) {
                case 1:
                    return functions['is_function/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('is_function' + ('/' + arguments.length));
        },
        'is_integer': function () {
            switch (arguments.length) {
                case 1:
                    return functions['is_integer/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('is_integer' + ('/' + arguments.length));
        },
        'is_list': function () {
            switch (arguments.length) {
                case 1:
                    return functions['is_list/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('is_list' + ('/' + arguments.length));
        },
        'is_map': function () {
            switch (arguments.length) {
                case 1:
                    return functions['is_map/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('is_map' + ('/' + arguments.length));
        },
        'is_number': function () {
            switch (arguments.length) {
                case 1:
                    return functions['is_number/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('is_number' + ('/' + arguments.length));
        },
        'is_pid': function () {
            switch (arguments.length) {
                case 1:
                    return functions['is_pid/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('is_pid' + ('/' + arguments.length));
        },
        'is_port': function () {
            switch (arguments.length) {
                case 1:
                    return functions['is_port/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('is_port' + ('/' + arguments.length));
        },
        'is_reference': function () {
            switch (arguments.length) {
                case 1:
                    return functions['is_reference/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('is_reference' + ('/' + arguments.length));
        },
        'is_tuple': function () {
            switch (arguments.length) {
                case 1:
                    return functions['is_tuple/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('is_tuple' + ('/' + arguments.length));
        },
        'abs': function () {
            switch (arguments.length) {
                case 1:
                    return functions['abs/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('abs' + ('/' + arguments.length));
        },
        'ceil': function () {
            switch (arguments.length) {
                case 1:
                    return functions['ceil/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('ceil' + ('/' + arguments.length));
        },
        'floor': function () {
            switch (arguments.length) {
                case 1:
                    return functions['floor/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('floor' + ('/' + arguments.length));
        },
        'trunc': function () {
            switch (arguments.length) {
                case 1:
                    return functions['trunc/1'](...arguments);
                    break;
            }
            throw '** exception error: undefined function' + ('trunc' + ('/' + arguments.length));
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

        for (var i = 0; i < str.length; i++) {
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

// Keep track of declared atoms
// todo: Expand to include reserved atoms
const atomTable = {};

// Constructor
function Atom(name) {
    this.value = name;

    if (!atomTable[name]) {
        atomTable[name] = this;
    }
}

// Static Methods
Atom.isAtom = (atom) => atom instanceof Atom;
Atom.cloneAtom = (atom) => atom;
Atom.exists = (name) => !!atomTable[name];

// Prototype Methods
Atom.prototype.getValue = function() {
    return this.value;
}

Atom.prototype.toString = function() {
    return this.value.toString();
}

Atom.prototype.isUnbound = function() {
    return false;
}

Atom.prototype.match = function(X) {
    // Since 'false' is a legitimate atom in Erlang, we return undefined instead of false for a failure case
    if (Atom.isAtom(X) ? this.value === X.value : this.value === X) {
        return X;
    }
    else {
        return undefined;
    }
}


if (typeof exports != "undefined") {
    exports.Atom = Atom;
}

// Constructor
function BitString () {
    var tmp = [];

    for (var i = 0; i < arguments.length; i++) {
        if (!(ErlNumber.isErlNumber(arguments[i]) && arguments[i].isInteger()) && !List.isString(arguments[i])) {
            throw "bad argument";
        }

        if (List.isString(arguments[i])) {
            for (c in arguments[i].toString().split("")) {
                tmp.push(c.charCodeAt(0));
            }
        }
        else {
            var max8bit = (1 << 8) - 1;
            tmp.push(arguments[i].greaterThan(max8bit) ? max8bit : arguments[i].getValue().c[0]);
        }
    }

    this.value = Uint8Array.of(...tmp);
}

// Static Methods
BitString.isBitString = (bitstring) => bitstring instanceof BitString;
BitString.isBinary = (bitstring) => bitstring instanceof BitString; // todo

// Prototype Methods
BitString.prototype.value = function() {
    return this.value;
};

BitString.prototype.toString = function() {
    return `<<${this.value.join(",")}>>`;
};

BitString.prototype.isUnbound = function() {
    return false;
};

BitString.prototype.match = function(X) {
    // Since 'false' is a legitimate atom in Erlang, we return undefined instead of false for a failure case
    if (BitString.isBitString(X) && this.value === X.value) {
        return X;
    }
    else {
        return undefined;
    }
};


if (typeof exports != "undefined") {
    exports.BitString = BitString;
}

// Constructor
function Fun() {
    this.value = undefined;
}

// Static Methods
Fun.isFun = (fun) => fun instanceof Fun;

// Prototype Methods
Fun.prototype.value = function() {
    throw "Fun is not implemented yet";
}

Fun.prototype.toString = function() {
    throw "Fun is not implemented yet";
}

Fun.prototype.isUnbound = function() {
    return false;
}

Fun.prototype.match = function(X) {
    // Since 'false' is a legitimate atom in Erlang, we return undefined instead of false for a failure case
    if (Fun.isFun(X) && this.value === X.value) {
        return X;
    }
    else {
        return undefined;
    }
}


if (typeof exports != "undefined") {
    exports.Fun = Fun;
}

// Constructor
function List (car, ...cdr) {
    if (typeof car == "string") {
        var chars = car.split("");

        for (var i = 0; i < chars.length; i++) {
            chars[i] = chars[i].charCodeAt(0);
        }

        this.value = new List(...chars);
    }
    else {
        this.value = car;
    }

    this.next = car !== undefined ? new List(...cdr) : undefined;
    this.iterator = this;
};


// Static Methods
List.isList = (list) => list instanceof List;

List.isEmptyList = (list) => List.isList(list) && list.value === undefined && list.next === undefined;

List.isString = (list) => {
    if (!List.isList(list)) {
        return false;
    }

    for (var i = 1; i <= list.size; i++) {
        if (!isLatin1Char(list.nth(i))) {
            return false;
        }
    }
    return true;
};

List.cloneList = (list) => new List(...list);


// Internal Usage Prototype Methods
List.prototype.__nthNode = function(n) {
    if (n < 0 || n >= this.size()) {
        throw "index out of bounds error";
    }    

    let i = 0;
    let walker = this;

    while (i < n) {
        walker = walker.next;
        i++;
    }

    return walker;
};

List.prototype[Symbol.iterator] = function() {
    return {
        next: () => {
            // If the next node of the current iterator isn't another list OR is an empty list, then we know
            // we have reached the end of the linked list
            let isLastNode = this.iterator.next === undefined || List.isEmptyList(this.iterator.next);
            let v = List.isList(this.iterator) ? this.iterator.value : this.iterator;
            
            if (this.iterator === "done" || List.isEmptyList(this)) {
                this.iterator = this;
                return {                    
                    done: true
                }
            }
            else {
                this.iterator = isLastNode ? "done" : this.iterator.next;
                return {
                    value: v,
                    done: false
                };
            }
        }
    }
};

List.prototype.__last = function() {
    return this.__nthNode(this.size() - 1);
};


// Prototype Methods
List.prototype.nth = function(n) {
    let nth = this.__nthNode(n);
    return List.isList(nth) ? nth.value : nth;
};

List.prototype.size = function() {
    return [...this].length;
};

List.prototype.cons = function(appendage) {
    let clone = List.cloneList(this);
    clone.__last().next = appendage;

    return clone;
};

List.prototype.value = function() {
    return [...this];  
};

List.prototype.toString = function() {
    let buffer = [...this];

    if (buffer.length) {
        let textBuffer = "";
        let isImproperList = !List.isList(this.__nthNode(Math.max(0, buffer.length - 1)));
        
        for (let i = 0; i < buffer.length; i++) {
            let isCharCode = Number.isInteger(buffer[i]) && isLatin1Char(buffer[i]);

            if (i > 0 && !(isCharCode && Number.isInteger(buffer[i - 1]) && isLatin1Char(buffer[i - 1]))) {
                if (i === buffer.length - 1 && isImproperList) {
                    textBuffer += "|";
                }
                else {
                    textBuffer += ",";
                }
            }

            if (isCharCode) {
                textBuffer += String.fromCharCode(buffer[i]);
            }
            else {
                textBuffer += buffer[i];
            }
        }

        return `[${textBuffer}]`;
    }
    
    return '[]';
}


// Private Methods
function isLatin1Char(c) {
    if (typeof c == "string") {
        c = c.charCodeAt(0);
    }
    return (c >= 32 && c <= 126) || (c >= 160 && c <= 255);
}


if (typeof exports != "undefined") {
    exports.List = List;
}

// Constructor
function ErlMap(map) {
    this.value = typeof map == "object" ? map : {};
}

// Static Methods
ErlMap.isErlMap = (map) => map instanceof ErlMap;

// Prototype Methods
ErlMap.prototype.getValue = function() {
    return this.value;
};

ErlMap.prototype.get = function(key) {
    if (typeof key != "string") {
        key = JSON.stringify(key);
    }

    return this.value[key];
};

ErlMap.prototype.put = function(key, value) {
    if (typeof key != "string") {
        key = JSON.stringify(key);
    }

    this.value[key] = value;
};

ErlMap.prototype.update = function(key, value) {
    if (typeof key != "string") {
        key = JSON.stringify(key);
    }

    if (this.value[key]) {
        this.value[key] = value;
    }
};

ErlMap.prototype.remove = function(key) {
    if (typeof key != "string") {
        key = JSON.stringify(key);
    }

    delete this.value[key];
};

ErlMap.prototype.size = function() {
    var size = 0, k;

    for (k in this.value) {
        if (this.value.hasOwnProperty(k)) {
            size++;
        }
    }

    return size;
};

// todo: Ensure keys are ordered as they are in erlang
ErlMap.prototype.toString = function() {
    var pairs = [], k;

    for (k in this.value) {
        if (this.value.hasOwnProperty(k)) {
            if (this.value[k] instanceof ErlMap) {
                pairs.push(k + "=>" + this.value[k].toString());
            } else {
                pairs.push(k + "=>" + JSON.stringify(this.value[k]));
            }
        }
    }

    return "#{" + pairs.join(",") + "}";
};

ErlMap.prototype.typeOf = function() {
    return "ErlMap";
};

ErlMap.prototype.isUnbound = function() {
    return false;
};

ErlMap.prototype.match = function(map) {
    // Since 'false' is a legitimate atom in Erlang, we return undefined instead of false for a failure case
    if (ErlMap.isErlMap(map) && this.equals(map.getValue())) {
        return map;
    } else {
        return undefined;
    }
};

if (typeof exports != "undefined") {
    exports.ErlMap = ErlMap;
}

var BigNumber = typeof BigNumber == "undefined" && typeof require == "function" ? require("bignumber.js") : BigNumber;


// Constructor
function ErlNumber(val) {
    this.value = val instanceof BigNumber ? val : new BigNumber(val);
}


// Static Methods
ErlNumber.isErlNumber = function(val) {
    return val instanceof ErlNumber;
}

ErlNumber.cloneNumber = function(val) {
    return new ErlNumber(val.getValue());
}


// Prototype Methods
ErlNumber.prototype.getValue = function() {
    return this.value;
}

ErlNumber.prototype.toString = function() {
    return this.value.toString();
}

ErlNumber.prototype.typeOf = function() {
    return "ErlNumber";
}

ErlNumber.prototype.isInteger = function() {
    return this.value.isInteger();
}

ErlNumber.prototype.isFloat = function () {
    return !this.value.isInteger();
}

ErlNumber.prototype.isUnbound = function() {
    return false;
}

ErlNumber.prototype.add = function(val) {
    return new ErlNumber(this.value.plus(val instanceof ErlNumber ? val.getValue() : val));
}

ErlNumber.prototype.subtract = function(val) {
    return new ErlNumber(this.value.minus(val instanceof ErlNumber ? val.getValue() : val));
}

ErlNumber.prototype.multiply = function(val) {
    return new ErlNumber(this.value.times(val instanceof ErlNumber ? val.getValue() : val));
}

ErlNumber.prototype.divide = function(val) {
    return new ErlNumber(this.value.div(val instanceof ErlNumber ? val.getValue() : val));
}

ErlNumber.prototype.intDivide = function(val) {
    return new ErlNumber(this.value.divToInt(val instanceof ErlNumber ? val.getValue() : val));
}

ErlNumber.prototype.remainder = function(val) {
    return new ErlNumber(this.value.mod(val instanceof ErlNumber ? val.getValue() : val));
}

ErlNumber.prototype.equals = function(val) {
    return this.value.equals(val instanceof ErlNumber ? val.getValue() : val);
}

ErlNumber.prototype.lessThan = function(val) {
    return this.value.lessThan(val instanceof ErlNumber ? val.getValue() : val);
}

ErlNumber.prototype.lessThanOrEq = function(val) {
    return this.value.lessThanOrEqualTo(val instanceof ErlNumber ? val.getValue() : val);
}

ErlNumber.prototype.greaterThan = function(val) {
    return this.value.greaterThan(val instanceof ErlNumber ? val.getValue() : val);
}

ErlNumber.prototype.greaterThanOrEq = function(val) {
    return this.value.greaterThanOrEqualTo(val instanceof ErlNumber ? val.getValue() : val);
}

ErlNumber.prototype.match = function(val) {
    if (isNaN(val) || !this.equals(val)) {
        return undefined;
    }
    return val;
}


if (typeof exports != "undefined") {
    exports.ErlNumber = ErlNumber;
}

// Pid testing
function Pid(arg) {
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
        this.id_a = Pid.rng(0, 32767); // Max is 15 bits
        this.id_b = Pid.rng(0, 7);     // Max is 3 bits
    }
}

// Static Methods
Pid.isPid = (pid) => pid instanceof Pid;

Pid.rng = (min, max) => {
    min = Math.ceil(min);
    max = Math.floor(max);
    return Math.floor(Math.random() * (max - min)) + min;
}; 

// Prototype Methods
Pid.prototype.value = function() {
    return this.id_a;
}

Pid.prototype.toString = function() {
    return `<${this.node}.${this.id_a}.${this.id_b}>`;
}

Pid.prototype.isUnbound = function() {
    return false;
}

Pid.prototype.match = function(X) {
    // Since 'false' is a legitimate atom in Erlang, we return undefined instead of false for a failure case
    if (Pid.isPid(X) && this.value === X.value) {
        return X;
    }
    else {
        return undefined;
    }
}
// Constructor
function Port() {
    this.value = undefined;
}

// Static Methods
Port.isPort = (port) => port instanceof Port;

// Prototype Methods
Port.prototype.value = function() {
    throw "Port is not implemented yet";
}

Port.prototype.toString = function() {
    throw "Port is not implemented yet";
}

Port.prototype.isUnbound = function() {
    return false;
}

Port.prototype.match = function(X) {
    // Since 'false' is a legitimate atom in Erlang, we return undefined instead of false for a failure case
    if (Port.isPort(X) && this.value === X.value) {
        return X;
    }
    else {
        return undefined;
    }
}


if (typeof exports != "undefined") {
    exports.Port = Port;
}

function Process(lambda, pidGenerator) {
    this.pid = pidGenerator !== undefined ? new Pid(pidGenerator) : new Pid();
    this.lambdas  = [lambda];
    this.currentLambda = false;
    this.messages = [];
    this.stack    = {};

    this.registerProcess();
}

// Static function to send messages to other processes by looking up pids in pid_map;
Process.sendMessage = (pid, msg) => {
    return Process.getProcess(pid).messages.push(msg);
};

// Static function to get a process from a pid
Process.getProcess = (pid) => {
    return __erlang.processes[__erlang.pids[pid.toString()]];
}

// Spawns a new process with a given lambda and pid generator,
// automatically registering it with the runtime process queue
Process.spawn = (lambda, pidGenerator) => {
    return __erlang.spawn(lambda, pidGenerator);
}

// Prototype Methods
Process.prototype.toString = function() {
    return this.pid.toString();
}

Process.prototype.registerProcess = function() {
    __erlang.pids[this.pid.toString()] = __erlang.processes.length;
    __erlang.processes.push(this);
}

/* TEST:
var p1 = Process.spawn(function() {
    if (this.messages.length) {
        let message = this.messages.shift();
        Process.sendMessage(message.pid, {pid: this.pid});
        console.log("PING: RECEIEVED MSG FROM " + message.pid);
    }
    this.lambdas.push(this.currentLambda);
});
var p2 = Process.spawn(function() {
    if (this.messages.length) {
        let message = this.messages.shift();
        Process.sendMessage(message.pid, {pid: this.pid});
        console.log("PING: RECEIEVED MSG FROM " + message.pid);
    }
    this.lambdas.push(this.currentLambda);
});
Process.sendMessage(p1, {pid: p2});
*/
// Constructor
function Reference() {
    this.value = ++Reference.last;
}

// Static Methods
Reference.isReference = (reference) => reference instanceof Reference;
Reference.last = 0;

// Prototype Methods
Reference.prototype.value = function() {
    return this.value;
}

Reference.prototype.toString = function() {
    return "#Ref<"+this.value.toString()+">";
}

Reference.prototype.isUnbound = function() {
    return false;
}

Reference.prototype.match = function(X) {
    // Since 'false' is a legitimate atom in Erlang, we return undefined instead of false for a failure case
    if (Reference.isReference(X) && this.value === X.value) {
        return X;
    }
    else {
        return undefined;
    }
}


if (typeof exports != "undefined") {
    exports.Reference = Reference;
}

// Constructor
function Tuple (car, ...cdr) {
    this.val = car;
    this.next = car !== undefined ? new Tuple(...cdr) : undefined;

    this.iterator = this;
};


// Static Methods
Tuple.isTuple = (tuple) => tuple instanceof Tuple;
Tuple.isEmptyTuple = (tuple) => Tuple.isTuple(tuple) && tuple.val === undefined && tuple.next === undefined;
Tuple.cloneTuple = (tuple) => new Tuple(...tuple);


// Internal Usage Prototype Methods
Tuple.prototype.__nthNode = function(n) {
    if (n < 0 || n >= this.size()) {
        throw "index out of bounds error";
    }    

    let i = 0;
    let walker = this;

    while (i < n) {
        walker = walker.next;
        i++;
    }

    return walker;
};

Tuple.prototype[Symbol.iterator] = function() {
    return {
        next: () => {
            //console.log(this, this.iterator);
            let v = this.iterator.val;
            
            if (this.iterator === undefined || Tuple.isEmptyTuple(this.iterator)) {
                this.iterator = this;
                return {                    
                    done: true
                }
            }
            else {
                this.iterator = this.iterator.next;
                return {
                    value: v,
                    done: false
                };
            }
        }
    }
};

Tuple.prototype.__last = function() {
    return this.__nthNode(this.size() - 1);
};


// Prototype Methods
Tuple.prototype.nth = function(n) {
    let nth = this.__nthNode(n);
    return Tuple.isTuple(nth) ? nth.val : nth;
};

Tuple.prototype.size = function() {
    return [...this].length;
};

Tuple.prototype.value = function() {
    return [...this];  
};

Tuple.prototype.toString = function() {
    return `{${[...this].join(",")}}`;
}


if (typeof exports != "undefined") {
    exports.Tuple = Tuple;
}

// Constructor 
function Unbound() {}

// Static Methods
Unbound.isUnbound = (X) => X instanceof Unbound;

// Prototype Methods
Unbound.prototype.toString = function() {
    throw `variable is unbound`;
}

Unbound.prototype.match = function(X) {
    return X;
}

Unbound.prototype.value = function() {
    return undefined;
}


if (typeof exports != "undefined") {
    exports.Unbound = Unbound;
}
