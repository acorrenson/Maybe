# Maybe

A tiny probabilist functional language

## Build instructions

This repository require a recent version of OCaml to be installed.
I recommend to install OCaml 4.14 via [opam]().

Once opam is correctly setup, the following commands should suffice to compile the project:
```
cd path/to/Maybe
opam install dune opal
dune build
```
## Usage

To execute a given probabilistic program written in a file `<file>.maybe` :

```
dune exec -- maybe <file>.maybe
```

To infer all possible results with the associated probabilities (inference) :

```
dune exec -- maybe -infer <file>.maybe
```

## The language

**Maybe** supports conditions, let-bindings, additions and sampling over simple distributions.
### Conditions

```
if <expr> {
  <expr>
} else {
  <expr>
}
```

### Let

```
let x = <expr> in <expr>
```

### Additions

```
<expr> + <expr>
```

### Sampling

Bernouilli variable (`1` with probability `p` and `0` with probability `1 - p`).
```
B(p)
```

Uniform choice over a list of integer constants
```
U[<int>, ..., <int>]
```

## Example

The following **Maybe** program sums to variables each following
a uniform distribution.

```
let x = U[1, 2, 3, 4] in
let y = U[1, 2, 3, 4] in
x + y
```

Running `maybe -infer` on this program gives:

```
2 ~ 0.062500
3 ~ 0.125000
4 ~ 0.187500
5 ~ 0.250000
6 ~ 0.187500
7 ~ 0.125000
8 ~ 0.062500
```
