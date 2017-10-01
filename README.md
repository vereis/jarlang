# jrlscript
An Erlang -> ES6 transpiler written in Erlang.

## Design Choices:

### Erlang Datatypes:
Because not all Erlang datatypes have unique corresponding datatypes in JavaScript, some datatypes such as Erlang's 'Integer' and 'Float' should be modelled with custom JavaScript classes. 

Due to this however, mathematical expressions such as addition and subtraction will have to be defined so that they know how to operate on said datatypes.

Characters in Erlang are actually just numbers, and strings are as logically follows, lists of numbers. We won't neccessarily be modelling this behaviour on the JavaScript side as characters and strings in Erlang do act like characters and strings in other programming languages with the only forseen caveat being that any functions which operate on lists and numbers now also apply to characters and strings.

As an optimisation step, we should represent characters and strings in Erlang as normal JavaScript strings until we detect that some kind of non-standard behaviour is being applied to them, in which case we can intelligently and lazily convert them into their corresponding list<int> representation.

A general and quick overview of how we will represent Erlang datatypes in JavaScript are as follows:

```
Integer    -> Custom Integer class
Float      -> Custom Float class
$char      -> String with length of 1
String     -> String where length > 1
Atom       -> Custom Atom class 
base#value -> Custom class wrapping an object such as {base: Number, value: Number} with a toInt() / toFloat() function
Bit Strings, Binary -> Custom class essentially wrapping an Array<Number>
Fun        -> Function
Port Identifier -> None // We don't have any special access to ports from JavaScript
PID        -> Custom class that roughly looks like {pid: Array<Number>} // Where Array is split by "."
Tuple      -> Custom Tuple class
Map        -> Standard Object
List       -> Custom List class
Records    -> Undecided
Boolean    -> Boolean
```

### Concurrency in JavaScript
While we haven't gotten to any point of development where we need to worry about implementing concurrency, it would be useful to make some initial decisions outlining potential paths to take when dealing with concurrency in JavaScript.

Because our goal is to run Erlang in any modern web browser AND NodeJS, we need to research how viable webworkers and service workers are when it comes to achieving this task.

Fake concurrency could possibly be obtained via manually handling concurrent stacks in some sort of array and manually running each stack one depth at a time to some clock interval. Processes need to be modelled as objects which can store their current state, a chain of arguments, parameters and local variables. IPC can be done simply by sending messages to them in the way of calling functions which push data into some kind of internal mail-box array. 

### Modelling function overloading / arity
Erlang essentially acts as though functions with differing numbers of parameters are different functions entirely, while overloaded functions with the same number of parameters are essentially different conditional branches of one function.

This can seemingly be modelled in JavaScript painlessly via having all Erlang modules exist as closures in JavaScript.

These closures will define two objects, an exports object and a functions object.

The exports object is similar to the export statement in Erlang. It is a fairly basic datastructure which reveals a function defined within the functions object. Any non-exported function then is not revealed to the global scope and thus cannot be called much like non-exported functions in Erlang modules.

The functions object is the key to modelling function arity however; the functions object can be modelled as a relatively straightforward datastructure where each key within the object is the name of a function. This key represents yet another object with keys representing the number of arguments provided to the function. These represent functions which then do conditional checking to determine what code should be executed.

A working example of this is detailed in the 'manual_translations/99bottles' directory. Example Erlang code and JavaScript exists fully functional. The readme there contains more information about this.

### Notices
While we will be generating ES6 style JavaScript, we shouldn't use features which cannot yet be transpiled down to ES5 via BabelJS. This is because we should aim to get this working on as many systems as possible. It shouldn't be particularly difficult to stick to the subset of transpilable ES6.
 
## Recommended Reading
I'm personally going through [Crafting Interpreters](http://www.craftinginterpreters.com/) and [Build Your Own Lisp](http://www.buildyourownlisp.com/) for learning and the material is quite good, I recommend following it.

Crafting Interpreters is a much higher level overview and gives us more insight into how to model a language with more complex syntax than a Lisp though the low level implementation of the language in it is incomplete.

Picking up the Dragon Compiler book is also a nice read.
