Samau
=====
Samau is a stack-based, concatenative, pure functional golfing language inspired by [Burlesque](http://mroman.ch/burlesque), [Joy](http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language), [Mathematica](http://wolfram.com/mathematica/), [Seriously](https://github.com/Mego/Seriously) and other languages. It is still under development, and only support a very limited amount of commands.

Some of the code is based on Roman Müntener's [Burlesque](https://github.com/FMNSSun/Burlesque), which is under [BSD license](https://github.com/FMNSSun/Burlesque#license).

The default character encoding of Samau is [CP737](https://en.wikipedia.org/wiki/Code_page_737), the DOS code page for Greek language. But that's just for counting bytes. There is also a UTF-8 mode, and the REPL uses UTF-8. You _don't_ need to set your terminal text encoding to CP737.

## Usage

To compile the Samau interpreter, you need the [Haskell Platform](https://www.haskell.org/platform/).

First, install the required packages:

    cabal install arithmoi haskeline parsec

[See this if you have problem installing `arithmoi`.](https://github.com/cartazio/arithmoi/issues/13)

Then compile it:

    ghc --make Main.hs -o samau

And then:

    ./samau code.sm           # execute the code from a CP737 encoded file
    ./samau --utf8 code.sm    # execute the code from a UTF-8 encoded file
    ./samau                   # start the REPL

Or you can run it without compiling:

    runghc Main.hs [args]

## Documentation
See the [wiki](https://github.com/AlephAlpha/Samau/wiki).

## Licence

Samau is licenced under [GNU GPL v3](https://github.com/AlephAlpha/Samau/blob/master/LICENSE).

Some of the code is based on Roman Müntener's [Burlesque](https://github.com/FMNSSun/Burlesque), which is under [BSD license](https://github.com/FMNSSun/Burlesque#license).

## Links
* [PPCG.se](http://codegolf.stackexchange.com/)
* [_The Legend of Samau_](http://www.guokr.com/blog/744130/)
