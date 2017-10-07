# Reverse engineering Core Erlang ASTs
Unsurprisingly, the AST generated from our compile to ast step is very easy to parse and very regular. So far, from fiddling around with it I think already we are at a point where we could build a very simple compiler to compile 'hello, world' tier Erlang.

## Pieces of the AST
Each 'node' of the AST is in the form:
```
{$NODE_TYPE, [], $ADDITIONAL}
```
Where each `$ADDITIONAL` is dependant on the `$NODE_TYPE`. Definitions for the node type seem to be defined in the `cerl.erl` module which you can find on [erldocs](http://erldocs.com/R16A/compiler/cerl.htm).

While I haven't proven this yet, just from hacking around, the empty list in the example is almost always empty. The list seems to store additional metadata but I've only seen it contain the atom `compiler_generated` to denote compiler generated code. As far as I know, the second index of all nodes contain this empty list.

### Module declaration
All files are aptly defined with a `c_module` node which is in the form:

```
{c_module, [], 
    {c_literal, [] $MODULE_NAME}, 
    $EXPORT_LIST,
    [],
    $FUNCTION_LIST}
}
```

### Exported Functions
`$EXPORT_LIST` takes the form:
```
[
    {c_var, [], {$FUNCTION_NAME, $FUNCTION_ARITY}},
    {c_var, [], {$ANOTHER_FUNCT, $FUNCTION_ARITY}},
    ...
]
```

### Function Definitions
The most interesting bit about the AST seems to be the `$FUNCTION_LIST` datastructure. The function list is a normal list containing a tuple which in turn contains two tuples:
```
{
    {c_var, [], {$FUNCTION_NAME, $FUNCTION_ARITY}},
    {c_fun, [], $FUNCTION_ARGS, $FUNCTION_BODY}
}
```
It appears that the first tuple is essentially what would appear if the function were exported. Since this is in a `c_var` node, we can assume that Erlang stores this function as a variable and simply exports it not too indifferently from our 99bottles.erl manual translation's closure export.

`$FUNCTION_ARGS` is a list of `c_var` nodes for use within the `$FUNCTION_BODY`.

### Function Bodies
While testing here was non-exhaustive, when given code that looks like the following:
```erlang
test_function() -> "hello, world!".
```
The function body datastructure simply looks like this:
```
{c_literal, [], "Hello, world!"}
```

From this, we can assume that Erlang implicitly whatever the function body is, and that the function body node isn't particularly special.

When I tried feeding the compiler slightly more complex code in the form of:
```erlang
test_function(TestVar) ->
    TestVar,
    NewVar = 12,
    "Hello, world!".
```
I was very confused, as the output didn't change. What this actually appears to be is the compiler optimising away unused variables, and variables which don't affect the variable at the end of the implicit return.

Doing something then to force more advanced code generation such as:
```erlang
test_function(TestVar) ->
    NewVar = 1 + 2,
    TestVar + 3.
```

Produces the following function body:
```
{c_call,[],
    {c_literal,[],erlang},
    {c_literal,[],'+'},
    [{c_var,[],'_cor0'},{c_literal,[],3}
```
It appears that the `c_call` node does most of the work in making Erlang functions actually perform work. The arguments are, ignoring the empty list, `$MODULE`, `$FUNCTION_NAME` and an `$ARGUMENTS_LIST`. I obviously suspect these to be infinitely nestable within reasonable limits.

The `c_var` `_cor0` here is just the parameter of the function, which would have appeared in the function parameter definition, so we can see a direct translation of our code, which is then implicitly returned.

An interesting note is that the NewVar variable we defined isn't defined in the function body anywhere, but instead is optimised into a `c_literal` node, which is interesting.