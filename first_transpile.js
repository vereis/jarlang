const core_AST_revEng = function () {
    'use_strict';
    const exports = {
        'boolean': function () {
            switch (arguments.length) {
            case 0:
                return functions['boolean/0'](arguments);
                break;
            }
            throw '** exception error: undefined function' + ('boolean' + ('/' + arguments.length));
        },
        'integer': function () {
            switch (arguments.length) {
            case 0:
                return functions['integer/0'](arguments);
                break;
            }
            throw '** exception error: undefined function' + ('integer' + ('/' + arguments.length));
        },
        'module_info': function () {
            switch (arguments.length) {
            case 0:
                return functions['module_info/0'](arguments);
                break;
            case 1:
                return functions['module_info/1'](arguments);
                break;
            }
            throw '** exception error: undefined function' + ('module_info' + ('/' + arguments.length));
        },
        'string': function () {
            switch (arguments.length) {
            case 0:
                return functions['string/0'](arguments);
                break;
            }
            throw '** exception error: undefined function' + ('string' + ('/' + arguments.length));
        }
    };
    const functions = {
        'boolean/0': function () {
            return 'function parsing not yet implemented';
        },
        'integer/0': function () {
            return 'function parsing not yet implemented';
        },
        'string/0': function () {
            return 'function parsing not yet implemented';
        },
        'module_info/0': function () {
            return 'function parsing not yet implemented';
        },
        'module_info/1': function () {
            return 'function parsing not yet implemented';
        }
    };
    return exports;
}();