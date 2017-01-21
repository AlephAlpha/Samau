# [Samau](https://github.com/AlephAlpha/Samau) Is Coming Back...

__Samau__ is a stack-based golfing language inspired by [Burlesque](http://mroman.ch/burlesque), [Joy](http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language) and other languages.

### Functions Implemented

Note that Samau uses [CP737](https://en.wikipedia.org/wiki/Code_page_737) encoding, though all the source codes are written in UTF-8, and the REPL is also in UTF-8. CP737 is only used for counting bytes.

| Function | CP737 | Description |
|:--------:|:-----:|:----------- |
| `!` | `0x21` | Discard the top of the stack |
| `*` | `0x2a` | Times |
| `+` | `0x2b` | Plus |
| `-` | `0x2d` | Minus |
| `/` | `0x2f` | Divide, always return a float |
| `:` | `0x3a` | [Cons](https://en.wikipedia.org/wiki/Cons) |
| `;` | `0x3b` | Duplicate the top of the stack |
| `d` | `0x64` | Pop a list `a` and an item `b`, execute `a` as a function, push `b` back. |
