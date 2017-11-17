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
        
        'format/1': function (_cor0) {
            console.log(_cor0);
            return 'ok';
        },
        'format/2': function (_cor1, _cor0) {
            console.log(_cor1);
            return 'ok';
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
