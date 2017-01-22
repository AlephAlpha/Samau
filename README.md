# [Samau](https://github.com/AlephAlpha/Samau) Is Coming Back...

__Samau__ is a stack-based golfing language inspired by [Burlesque](http://mroman.ch/burlesque), [Joy](http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language) and other languages.

## Built-in Functions

Samau uses [CP737](https://en.wikipedia.org/wiki/Code_page_737) encoding for counting bytes, and UTF-8 in other cases (e.g., when you are actually writing a Samau program).

| Char | CP737 | Description |
|:----:|:-----:|:----------- |
| `!` | `0x21` | Discard the top of the stack |
| `$` | `0x24` | Swap the top two items of the stack |
| `*` | `0x2a` | Times |
| `+` | `0x2b` | Plus |
| `-` | `0x2d` | Minus |
| `/` | `0x2f` | Divide, always returns a float |
| `:` | `0x3a` | [Cons](https://en.wikipedia.org/wiki/Cons) |
| `;` | `0x3b` | Duplicate the top of the stack |
| `^` | `0x5e` | Power |
| `d` | `0x64` | Pop a list `a` and an item `b`, execute `a` as a function, push `b` back |
| `i` | `0x69` | Pop a list `a`, execute `a` as a function |

## Usage

You need [Haskell](https://www.haskell.org/) and [Stack](https://www.haskellstack.org/).

Compile Samau:
```
git clone https://github.com/AlephAlpha/Samau.git
cd Samau
stack setup
stack build
```

Run the REPL:
```
stack exec samau
```

Run the code from a file:
```
stack exec samau filename
```
