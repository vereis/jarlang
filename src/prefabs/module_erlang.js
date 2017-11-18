const erlang = function () {
    'use_strict';
    const exports = {
        
        
        'addition': function () {
            switch (arguments.length) {
            case 2:
                return functions['addition/2'](...arguments);
                break;
            }
            throw '** exception error: undefined function' + ('addition' + ('/' + arguments.length));
        },
        'subtraction': function () {
            switch (arguments.length) {
            case 2:
                return functions['subtraction/2'](...arguments);
                break;
            }
            throw '** exception error: undefined function' + ('subtraction' + ('/' + arguments.length));
        },
        'multiplication': function () {
            switch (arguments.length) {
            case 2:
                return functions['multiplication/2'](...arguments);
                break;
            }
            throw '** exception error: undefined function' + ('multiplication' + ('/' + arguments.length));
        },
        'division': function () {
            switch (arguments.length) {
            case 2:
                return functions['division/2'](...arguments);
                break;
            }
            throw '** exception error: undefined function' + ('division' + ('/' + arguments.length));
        },
        'remainder': function () {
            switch (arguments.length) {
            case 2:
                return functions['remainder/2'](...arguments);
                break;
            }
            throw '** exception error: undefined function' + ('remainder' + ('/' + arguments.length));
        },
        'intDivision': function () {
            switch (arguments.length) {
            case 2:
                return functions['intDivision/2'](...arguments);
                break;
            }
            throw '** exception error: undefined function' + ('intDivision' + ('/' + arguments.length));
        },
        
        
        
        'equality': function () {
            switch (arguments.length) {
            case 2:
                return functions['equality/2'](...arguments);
                break;
            }
            throw '** exception error: undefined function' + ('equality' + ('/' + arguments.length));
        },
        'notEquality': function () {
            switch (arguments.length) {
            case 2:
                return functions['notEquality/2'](...arguments);
                break;
            }
            throw '** exception error: undefined function' + ('notEquality' + ('/' + arguments.length));
        },
        'lessThan': function () {
            switch (arguments.length) {
            case 2:
                return functions['lessThan/2'](...arguments);
                break;
            }
            throw '** exception error: undefined function' + ('lessThan' + ('/' + arguments.length));
        },
        'lessThanOrEq': function () {
            switch (arguments.length) {
            case 2:
                return functions['lessThanOrEq/2'](...arguments);
                break;
            }
            throw '** exception error: undefined function' + ('lessThanOrEq' + ('/' + arguments.length));
        },
        'moreThan': function () {
            switch (arguments.length) {
            case 2:
                return functions['moreThan/2'](...arguments);
                break;
            }
            throw '** exception error: undefined function' + ('moreThan' + ('/' + arguments.length));
        },
        'moreThanOrEq': function () {
            switch (arguments.length) {
            case 2:
                return functions['moreThanOrEq/2'](...arguments);
                break;
            }
            throw '** exception error: undefined function' + ('moreThanOrEq' + ('/' + arguments.length));
        },
        'exactlyEq': function () {
            switch (arguments.length) {
            case 2:
                return functions['exactlyEq/2'](...arguments);
                break;
            }
            throw '** exception error: undefined function' + ('exactlyEq' + ('/' + arguments.length));
        },
        'exactlyNotEq': function () {
            switch (arguments.length) {
            case 2:
                return functions['exactlyNotEq/2'](...arguments);
                break;
            }
            throw '** exception error: undefined function' + ('exactlyNotEq' + ('/' + arguments.length));
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
        
        'addition/2': function (_cor1, _cor0) {
            if(ErlNumber.isErlNumber(_cor1) && ErlNumber.isErlNumber(_cor0)){
                return _cor1.add(_cor0);
            }
            throw '** exception error: an error occurred when evaluating an arithmetic expression in operator +/2 called as '+_cor1+' + '+_cor0;
        },
        'subtraction/2': function (_cor1, _cor0) {
            if(ErlNumber.isErlNumber(_cor1) && ErlNumber.isErlNumber(_cor0)){
                return _cor1.subtract(_cor0);
            }
            throw '** exception error: an error occurred when evaluating an arithmetic expression in operator -/2 called as '+_cor1+' - '+_cor0;
        },
        'multiplication/2': function (_cor1, _cor0) {
            if(ErlNumber.isErlNumber(_cor1) && ErlNumber.isErlNumber(_cor0)){
                return _cor1.multiply(_cor0);
            }
            throw '** exception error: an error occurred when evaluating an arithmetic expression in operator */2 called as '+_cor1+' * '+_cor0;
        },
        'division/2': function (_cor1, _cor0) {
            if(ErlNumber.isErlNumber(_cor1) && ErlNumber.isErlNumber(_cor0)){
                return _cor1.divide(_cor0);
            }
            throw '** exception error: an error occurred when evaluating an arithmetic expression in operator \'/\'/2 called as '+_cor1+' / '+_cor0;
        },
        'remainder/2': function (_cor1, _cor0) {
            if(ErlNumber.isErlNumber(_cor1) && ErlNumber.isErlNumber(_cor0)){
                return _cor1.remainder(_cor0);
            }
            throw '** exception error: an error occurred when evaluating an arithmetic expression in operator rem/2 called as '+_cor1+' rem '+_cor0;
        },
        'intDivision/2': function (_cor1, _cor0) {
            if(ErlNumber.isErlNumber(_cor1) && ErlNumber.isErlNumber(_cor0)){
                return _cor1.intDivide(_cor0);
            }
            throw '** exception error: an error occurred when evaluating an arithmetic expression in operator div/2 called as '+_cor1+' div '+_cor0;
        },
        
        
        
        'equality/2': function (_cor1, _cor0) {
            return _cor1 == _cor0;
        },
        'notEquality/2': function (_cor1, _cor0) {
            return _cor1 != _cor0;
        },
        'lessThan/2': function (_cor1, _cor0) {
            return _cor1 < _cor0;
        },
        'lessThanOrEq/2': function (_cor1, _cor0) {
            return _cor1 <= _cor0;
        },
        'moreThan/2': function (_cor1, _cor0) {
            return _cor1 > _cor0;
        },
        'moreThanOrEq/2': function (_cor1, _cor0) {
            return _cor1 >= _cor0;
        },
        'exactlyEq/2': function (_cor1, _cor0) {
            return _cor1 === _cor0;
        },
        'exactlyNotEq/2': function (_cor1, _cor0) {
            return _cor1 !== _cor0;
        },
        
        
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
        }
    };
    return exports;
}();
