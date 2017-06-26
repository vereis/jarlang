# jrlscript
An Erlang -> ES6 transpiler written in Erlang.

## Contributing
Just do a clone and pull. I'll review changes and merge anything good.
Snippets of functions for translation can be found at the following repo, let's keep these seperate:
[JS high level function snippets](https://github.com/Vereis/jrlscript-arr-fns)

## Goals:
- Be able to transpile non-trivial Erlang code to ES6 to run in both Node and Browser
- Fake concurrency via Web Workers, falling back to co-processing and faux context switching where neccessary
- The ability to bootstrap itself so that we end up having a jrlscript implementation in both ES6 and Erlang. This means that we should be able to evaluate Erlang in real time on the browser.

## Design Choices:
- We will model Erlang Datatypes as follows:
    ```
    Integer    -> Number
    Float      -> Number
    $char      -> Number
    base#value -> Object that looks like {base: Number, value: Number}
    Atom       -> String
    Bit Strings, Binary -> Array<Number>
    Fun        -> Function
    Port Identifier -> None
    PID        -> Object that looks like {pid: Array<Number>} // Where Array is split by "."
    Tuple      -> Custom Tuple Object
    Map        -> Object
    List       -> Custom List Object
    String     -> Array<Integer> // For compatability with Erlang's quirkiness
    Records    -> Undecided
    Boolean    -> Boolean
    ```
- We can possibly model concurrency as follows:
    ```
      1) Store all active processes in an array of processes
      2) Have a clock interval which iterates through all processes and runs them
      3) Process entries store current stack, along with native JS Functions
      4) Because functions should be pure, we can possibly assume each function call in a process is isolate and thus all the information we will need to store will be present in the current stack frame?
    ```
- We're targeting ES6 so we can integrate something like BabelJS to transpile down to lower and more supported ES specifications

## Recommended Reading
I'm personally going through [Crafting Interpreters](http://www.craftinginterpreters.com/) and [Build Your Own Lisp](http://www.buildyourownlisp.com/) for learning and the material is quite good, I recommend following it.

Crafting Interpreters is a much higher level overview and gives us more insight into how to model a language with more complex syntax than a Lisp though the low level implementation of the language in it is incomplete.
