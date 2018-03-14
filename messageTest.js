/tmp/jarlang/node_modules/escodegen/escodegen.js:2472
        result = this[stmt.type](stmt, flags);
                                ^

TypeError: this[stmt.type] is not a function
    at CodeGenerator.generateStatement (/tmp/jarlang/node_modules/escodegen/escodegen.js:2472:33)
    at /tmp/jarlang/node_modules/escodegen/escodegen.js:1083:51
    at withIndent (/tmp/jarlang/node_modules/escodegen/escodegen.js:582:9)
    at CodeGenerator.BlockStatement (/tmp/jarlang/node_modules/escodegen/escodegen.js:1033:13)
    at CodeGenerator.generateStatement (/tmp/jarlang/node_modules/escodegen/escodegen.js:2472:33)
    at CodeGenerator.maybeBlock (/tmp/jarlang/node_modules/escodegen/escodegen.js:837:33)
    at CodeGenerator.IfStatement (/tmp/jarlang/node_modules/escodegen/escodegen.js:1629:34)
    at CodeGenerator.generateStatement (/tmp/jarlang/node_modules/escodegen/escodegen.js:2472:33)
    at /tmp/jarlang/node_modules/escodegen/escodegen.js:1083:51
    at withIndent (/tmp/jarlang/node_modules/escodegen/escodegen.js:582:9)
