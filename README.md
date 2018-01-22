# jarlang
Jarlang is an Erlang compiler which outputs to ES6 spec JavaScript, written in a combination of Erlang and JavaScript (NodeJS).

## Overview
Jarlang is implemented via the following, simple, pipeline:
1) Utilising the Erlang compiler, we generate Erlang's intermediate language, CoreErlang
2) Utilise built in Erlang tools to generate an abstract syntax tree from generated CoreErlang, not too disimilarly from projects such as [LuvvieScript](https://github.com/hypernumbers/LuvvieScript) and [ElixirScript](https://github.com/elixirscript/elixirscript)
3) Map the generated CoreErlang abstract syntax tree into an equivalent JavaScript abstract syntax tree
4) Utilise NodeJS and tools such as [escodegen](https://github.com/estools/escodegen) to generate valid JavaScript.

Our JavaScript AST is based off the documentation you can find here: [JSTree AST](https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Parser_API).

## Getting Started
Jarlang is still in development and can't at this time can only transpile trivial pieces of Erlang to JavaScript. The following instructions will help you get an environment set up for development and testing purposes only. This tool is not at all ready for any real world usage.

### Requirements
- Erlang 18 (Other versions may work, but any development and testing so far has only been done using version 18)
- NodeJS v8.8.0 (Other versions may work, but any development and testing so far has only been done using v8.8.0)

### Installing
1) Clone the repo with ```git clone https://github.com/vereis/jarlang``` on the commandline which should create a ```jarlang``` folder containing all our source code
2) Go into the ```jarlang``` folder with ```cd jarlang```
3) Get required NodeJS packages with the command ```npm install```
4) Run the ```make``` or ```make debug``` commands which will build either the release build of Jarlang or the debug build of Jarlang respectively. 
5) Run ```jarlang``` with the command ```ebin/jarlang``` or ```edebug/jarlang``` followed by paths to erlang files to compile into JavaScript. Additional info is available if you run the command ```jarlang -h```.

### Automatic Testing
Run the command ```make test``` which will compile the project into the ```etesting``` directory and run the following tests:
- Eunit tests in all modules where we have tests implemented
- Dialyzer performing static analysis of Jarlang
- Elvis to lint our code to confirm to a given style

Eventually, we will need to add some rudimentary testing on the JavaScript side, as well as testing to ensure the results of running Erlang code and transpiled JavaScript code are functionally the same.

## Contribution Guidelines
Please ensure automatic testing passes, when implemented, before pushing commits. Otherwise, try to ensure you can successfully run ```make``` to build ```Jarlang``` successfully before pushing commits. Please DON'T BREAK THE BUILD.

## Acknowledgements
- The escodegen guys for making generating JavaScript code so painless.
- The people who documented the EStree specification. Thank you. So much.
