# [Samau](https://github.com/AlephAlpha/Samau) Is Coming Back...

__Samau__ is a stack-based golfing language inspired by [Burlesque](http://mroman.ch/burlesque), [Joy](http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language) and other languages.

## Types

There are five types in Samau: integers, floats, characters, operators and lists. In integers and floats, the negative sign is `_` instead of `-`. Characters are written with a single quote, e.g. `'A`, `'0`. Operators are built-in functions. Lists are written inside square brackets. There is no string. Use lists of characters instead.

## Input and Output

Samau is a purly functional language. There is no input or output.

## Built-in Functions

Samau uses [CP737](https://en.wikipedia.org/wiki/Code_page_737) encoding for counting bytes, and UTF-8 in other cases (e.g., when you are actually writing a Samau program).

| Char | CP737 | Description |
|:----:|:-----:|:----------- |
| `!` | `0x21` | Discard the top of the stack |
| `$` | `0x24` | Swap the top two items of the stack |
| `&` | `0x24` | And |
| `(` | `0x28` | -1 |
| `)` | `0x29` | +1 |
| `*` | `0x2a` | Times |
| `+` | `0x2b` | Plus |
| `-` | `0x2d` | Minus |
| `.` | `0x2e` | [Concat](https://en.wikipedia.org/wiki/Concatenation) |
| `/` | `0x2f` | Divide, always returns a float |
| `:` | `0x3a` | [Cons](https://en.wikipedia.org/wiki/Cons) |
| `;` | `0x3b` | Duplicate the top of the stack |
| `<` | `0x3c` | Less than |
| `=` | `0x3d` | Test if two items are the same |
| `>` | `0x3e` | Greater than |
| `@` | `0x40` | Pop `a`, pop `b`, pop `c`, push `b`, puah `a`, push `a` |
| `^` | `0x5e` | Power |
| `_` | `0x5f` | Take the [oposite number](https://en.wikipedia.org/wiki/Additive_inverse) |
| `d` | `0x64` | Pop a list `a` and an item `b`, execute `a` as a function, push `b` back |
| `i` | `0x69` | Pop a list `a`, execute `a` as a function |
| `m` | `0x6d` | [Map](https://en.wikipedia.org/wiki/Map_(higher-order_function)) |
| `t` | `0x74` | Pop a list `a` and an item `b`, execute `a` as a function, push `b` back, execute `a` again |
| `z` | `0x7a` | [ZipWith](https://en.wikipedia.org/wiki/Map_(higher-order_function)) |
| `|` | `0x7c` | Or |

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
