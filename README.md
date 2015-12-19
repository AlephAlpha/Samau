Samau
=====
Samau is a stack-based, concatenative, pure functional golfing language inspired by [Burlesque](https://github.com/FMNSSun/Burlesque), [Joy](http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language), [Mathematica](http://wolfram.com/mathematica/), [Seriously](https://github.com/Mego/Seriously) and other languages.

The default character encoding of Samau is [CP737](https://en.wikipedia.org/wiki/Code_page_737), the DOS code page for Greek language. But that's just for counting bytes. There is also a UTF-8 mode, and the REPL uses UTF-8.

Usage
=====

To compile the Samau interpreter, you need the [Haskell Platform](www.haskell.org/platform/).

First, install the required packages:

    cabal install arithmoi haskeline parsec

Then compile it:

    ghc --make main.hs -o samau

And then:

    ./samau code.sm           # execute the code from a CP737 encoded file
    ./samau --utf8 code.sm    # execute the code from a UTF-8 encoded file
    ./samau                   # start the REPL

Or you can run it without compiling:

    runghc main.hs [args]

Links
=====
* [PPCG.se](http://codegolf.stackexchange.com/)
* [_The Legend of Samau_](http://www.guokr.com/blog/744130/)
