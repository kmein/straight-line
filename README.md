# straight-line
*A small non-Turing-complete programming language*

## Syntax
```
<program> ::= <instruction>
            | <instruction> ";" <program>

<instruction> ::= <variable> ":=" <expression>

<expression> ::= <constant>
               | <variable> "+" <variable>
               | <variable> "*" <variable>

<variable> ::= "o"
             | "i0" | "i1" | "i2" | ...
             | "x0" | "x1" | "x2ʺ | ...

<constant> ::= 0 | 1 | 2 | ...
```

## Semantics
All programs *P* in Straight-Line model a function *f : ℕ<sup>k</sup> → ℕ*. This function
is calculated as follows:
* The variables `i0`, `i1`, ... are input parameters, they have to be set when
  the program is run.
* All other variables are initialised to `0`.
* `x := c` assigns a constant `c` to a variable `x`.
* `x := a + b` and `x := a * b` assign the sum/product of variables `a` and
  `b` to an variable `x`.
* The function's result is the content of the variable `o`.

## Example
The following program (source code [here](/examples/mystery.sline)) calculates the function *f(x, y) = x²y² + x²*:

```
x0 := i0 * i0;
i1 := i1 * i1;
o := x0 * i1;
o := o + x0
```
