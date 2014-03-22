# gen-java-src

ClojureScript library designed to just generate java source files working with
static int functions and variables. It is not notably smart, but at least it
produces terminating, working code.

The generation is used as an example of how to generate specialized values using
Clojure's `test.check` functionality. As `test.check` is just for Clojure, I'm
using the [double-check](https://github.com/cemerick/double-check) port.

## Usage

The namespace `gen-java-src.main` has a function `htmlized-class` which returns
an html string with the class. The input value, `n`, is a number to set the
complexity of the program.

## License

Copyright © 2014 Jean Niklas L'orange

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
