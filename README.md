# jarlang
Jarlang is an Erlang to JavaScript transpiler, which itself is mostly written in Erlang. Our main goal is to be able to support as much of Erlang as possible and not just focus on being able to transpile a subset of it. A long term goal of the project is to be able to transpile the transpiler itself, and be able to write Erlang in the browser.

Jarlang works similarly to other similar projects such as [LuvvieScript](https://github.com/hypernumbers/LuvvieScript) and [ElixirScript](https://github.com/elixirscript/elixirscript) by performing a sideways translation from some AST representing Erlang code to the documented [JSTree AST](https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Parser_API). We then offload the work of actually generating JavaScript to projects such as [escodegen](https://github.com/estools/escodegen).

## Getting Started
Jarlang is still in development and can't at this time generate any real JavaScript. The following instructions will help you get an environment set up for development and testing purposes only.

### Requirements
- Erlang 18 (Other versions may work, but any development and testing so far has only been done using 18)
- NodeJS v8.8.0 (Other versions may work, but any developmetn and testing so far has only been done using v8.8.0)

### Installing
We will be moving to [rebar3](https://github.com/erlang/rebar3) as a build tool in the future but for now, installation instructions are as follows:
1) Clone the repo with ```git clone https://github.com/vereis/jarlang``` on the commandline which should create a ```jarlang``` folder
2) Go into the ```jarlang``` folder with ```cd jarlang```
3) You can run either ```make``` or ```make debug``` to automatically compile everything needed to get up and running. If ```make``` is successful, it'll dump the compiled files into an ```ebin``` directory. Likewise, if ```make debug``` was successful, it'll dump the compiled files into an ```edebug``` directory.
4) Currently, running the transpiler has to be done from the ```Erlang Shell```

### Automatic Testing
Not yet implemented

## Contribution Guidelines
Please ensure automatic testing passes, when implemented, before pushing commits. Otherwise, try to ensure you can successfully run ```make``` to build ```Jarlang``` successfully before pushing commits.

Once we've migrated to ```rebar3``` as a build tool, we should aim to enforce standardised programming style and convention which will be outlined below.

## Versioning
We use [SemVer](http://semver.org/) for versioning.

## Acknowledgements
- The escodegen guys for making generating JavaScript code so painless.
- The people who documented the EStree specification. Thank you. So much.
