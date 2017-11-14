# jarlang
Jarlang is an Erlang to JavaScript transpiler, which itself is mostly written in Erlang. Our main goal is to be able to support as much of Erlang as possible and not just focus on being able to transpile a subset of it. A long term goal of the project is to be able to transpile the transpiler itself, and be able to write Erlang in the browser.

Jarlang works similarly to other similar projects such as [LuvvieScript](https://github.com/hypernumbers/LuvvieScript) and [ElixirScript](https://github.com/elixirscript/elixirscript) by performing a sideways translation from some AST representing Erlang code to the documented [JSTree AST](https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Parser_API). We then offload the work of actually generating JavaScript to projects such as [escodegen](https://github.com/estools/escodegen).

## Getting Started
Jarlang is still in development and can't at this time can only transpile trivial pieces of Erlang to JavaScript. The following instructions will help you get an environment set up for development and testing purposes only. This tool is not at all ready for any real world usage.

### Requirements
- Erlang 18 (Other versions may work, but any development and testing so far has only been done using 18)
- NodeJS v8.8.0 (Other versions may work, but any developmetn and testing so far has only been done using v8.8.0)

### Installing
1) Clone the repo with ```git clone https://github.com/vereis/jarlang``` on the commandline which should create a ```jarlang``` folder containing all our source code
2) Go into the ```jarlang``` folder with ```cd jarlang```
3) Run the ```make``` or ```make debug``` commands which will build either the release build of Jarlang or the debug build of Jarlang respectively. 
4) Follow the instructions given to you from the result of ```make``` which will tell you to run ```jarlang.sh``` followed by a list of files to transpile as an argument. The folder containing ```jarlang.sh``` will depend on the type of build ```make``` has built.
5) Currently, the generated JavaScript is printed to the command line. It does not currently write resultant JavaScript to a file.

### Automatic Testing
Run the command ```make test``` which will compile the project into the ```etesting``` directory and run the following tests:
- Eunit tests in all modules if specified
- XRef analyses of module code
- Dialyzer performing static analysis of Jarlang

Eventually, we will need to add some rudimentary testing on the JavaScript side, as well as testing to ensure the results of running Erlang code and transpiled JavaScript code are functionally the same.

## Contribution Guidelines
Please ensure automatic testing passes, when implemented, before pushing commits. Otherwise, try to ensure you can successfully run ```make``` to build ```Jarlang``` successfully before pushing commits. Please DON'T BREAK THE BUILD.

Eventually, we'll be integrating [Elvis](https://github.com/inaka/elvis) to detect and enforce code style.


## Versioning
We use [SemVer](http://semver.org/) for versioning.

## Acknowledgements
- The escodegen guys for making generating JavaScript code so painless.
- The people who documented the EStree specification. Thank you. So much.
