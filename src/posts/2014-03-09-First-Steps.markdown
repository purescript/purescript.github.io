---
title: First Steps With PureScript - Solving Project Euler #1
author: Phil Freeman
---

Welcome to the PureScript community blog! In this first post, I'm going to walk through the basics of getting set up to use the PureScript compiler `psc`, and its interactive mode `psci`.

I'll start with the installation of the compiler, go through the basic commands of `psc` and `psci`, working towards a solution of problem 1 from [Project Euler](http://projecteuler.net/problem=1).

### Installing the Compiler

PureScript can be installed from [Hackage](http://hackage.haskell.org/package/purescript). After installing the [Haskell Platform](http://www.haskell.org/platform), you can use the following to build the compiler from source:

    cabal update
    cabal install purescript

### Working in PSCI

`psci` is the interactive mode of PureScript. It is useful for working with pure computations, and for testing ideas.

`psci` uses `nodejs` to execute compiled Javascript, so if you do not have that installed already, you will need to [install it now](http://nodejs.org).

Open `psci` by typing `psci` at the command line.

     ____                 ____            _       _   
    |  _ \ _   _ _ __ ___/ ___|  ___ _ __(_)_ __ | |_ 
    | |_) | | | | '__/ _ \___ \ / __| '__| | '_ \| __|
    |  __/| |_| | | |  __/___) | (__| |  | | |_) | |_ 
    |_|    \__,_|_|  \___|____/ \___|_|  |_| .__/ \__|
                                         |_|        
    
    :? shows help

    Expressions are terminated using Ctrl+D
    >

As the introduction indicates, you can type `:?` to see a list of commands:

    The following commands are available:
    
        :?              Show this help menu
        :i <module>     Import <module> for use in PSCI
        :m <file>       Load <file> for importing
        :q              Quit PSCi
        :r              Reset
        :t <expr>       Show the type of <expr>

We will use a selection of these commands during this tutorial.

Start by pressing the Tab key to use the autocompletion feature. You will see a collection of names of functions from the Prelude which are available to use.

To see the type of one of these values, type the `:t` command, followed by a space, followed by the name of the value:

    > :t Data.Array.foldl
    forall a b. (b -> a -> b) -> b -> [a] -> b
    > :t Data.Tuple.curry
    forall a b c. (Data.Tuple.Tuple a b -> c) -> a -> b -> c

We will be using some of the functions from the `Data.Array` module, so import that module by using the `:i` command:

    :i Data.Array

Note that using `Tab` to autocomplete names can be a useful time-saving device in `psci`.

### Solving Project Euler #1

The following problem is taken from [Project Euler](http://projecteuler.net/problem=1):
 
> If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
>
> Find the sum of all the multiples of 3 or 5 below 1000.

We can solve this problem neatly using functions and function composition, directly in `psci`.

Let's start by listing all of the natural numbers below 1000 as an array. We can do this using the `range` function from `Data.Array`:

    > range 0 999
    [0,1,2,3,4,...

You should see an array with 1000 elements printed to the command line.

This value can be given a name, using a `let` binding:

    > let ns = range 0 999

Now let's filter out all of those elements which do not meet the criterion. We can use the `filter` function from `Data.Array`, by providing a predicate function as its first argument:

    > let multiples = filter (\n -> n % 3 == 0 || n % 5 == 0) ns

You can see the result by evaluating `multiples` if you like, or even check its type:

    > multiples
    [0,3,5,6,9,10,12,15,...
    > :t multiples
    [Prim.Number]

Now we need to find the sum of the `multiples` array, to complete the solution.

There is no `sum` function in the Prelude, but we can write one for ourselves, using the `foldl` combinator from `Data.Array`:

    > let sum = foldl (+) 0

`foldl` takes a function for combining values, and an initial value, and combines all values in an array, moving from left to right.

We can try our new function on a few small arrays:

    > sum [] 
    0
    > sum [1,2,3]  
    6

Finally, let's use the `sum` function to find the sum of the `multiples` array:

    > sum multiples
    233168

When you have finished using `psci`, type `:q` to quit:

    > :q
    See ya!

### Compiling a Solution

Now that we've seen how to use `psci` to reach the answer, let's move our solution into a source file, and compile it using `psc`.

Create a new text file `Euler.purs` and copy the following code:

    module Euler1 where

    import Prelude
    import Data.Array

    sum = foldl (+) 0
    
    ns = range 0 999

    multiples = filter (\n -> n % 3 == 0 || n % 5 == 0) ns

    answer = sum multiples

It is possible to load this file directly into `psci` and to continue working:

    psci Euler1.purs
    > Euler1.answer
    233168
    > :q
    See ya!

Alternatively, we can use the compiler `psc` to compile the `Euler1.purs` file to Javascript.

    psc Euler1.purs

You should see a lot of Javascript printed onto the console. The reason that there is so much output is that the entire Prelude is being included with your program. Dead code elimination can be used to remove any code which is not required by your program. Use the `--module` command line option, specifing the module names of the modules whose code is required:

    psc Euler1.purs --module Euler1

This time you should see only the Prelude functions which you have used, namely `foldl`, `filter` and `range`.

If you would like to save the generated Javascript to a file instead printing it to the console, specify a file using the `-o` option:

    psc Euler1.purs --module Euler1 -o Euler1.js

If you load this file in `node`, you will be able to evaluate its expressions there as well:

    node
    > var euler1 = require("Euler1.js")
    > euler1.Euler1.answer
    233168

This time, let's define a `Main` module, so that we can create an executable Javascript file, which will print the answer to the console.

Create a new file, `Main.purs`, and copy the following code:

    module Main where

    import Prelude
    import Euler1 

    main = Debug.Trace.trace $ "The answer is " ++ show answer

Executable PureScript files must define a value `Main.main`. They are compiled with the `--main` option, which generates code to call `Main.main`:

    psc Euler1.purs Main.purs --module Main --main -o Main.js

Note that this time, we use the `Main` module as the entry point for the purpose of dead code elimination.

When the compiler finishes generating `Main.js`, simply run it on the command line to see the result:

    node Main.js
    The answer is 233168

### Conclusion

That's all for this post. We've seen how to use enough of the basics of `psc` and `psci` to compile and execute simple PureScript programs. If you would like more information, the [PureScript documentation](http://docs.purescript.org) lists all of the options for both `psc` and `psci`.

Until next time\...
