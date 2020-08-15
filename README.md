![](https://github.com/seanwestfall/hm/blob/master/img/hm.png)  
Robin Milner and J. Roger Hindley  

# hm
An implementation of the Hindley-Milner type system ([wikipedia](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)) in a simple Scheme/Lisp like language.

# theory
![https://youtu.be/x3evzO8O9e8](https://raw.githubusercontent.com/seanwestfall/hm/master/img/spj_video.png)
Simon Peyton Jones how GHC type inference engine actually works [Watch Here](https://youtu.be/x3evzO8O9e8)  
A Practical Type Checker for Scheme by Christian Lindig [Read Here](https://github.com/seanwestfall/scientific_papers/blob/master/papers/10.1.1.56.9923.pdf)  
The Design and Implementation of Typed Scheme by Sam Tobin-Hochstadt and Mathhias Felleisen [Read Here](https://github.com/seanwestfall/scientific_papers/blob/master/papers/popl08-thf.pdf)  

# build
```bash
# ghc -o dist/hm --make src/hs.hs
# ./dist/hm ...
```

# Cabal
```bash
# cabal build
# ./dist-newstyle/build/x86_64-osx/ghc-8.6.5/hm-0.1.0.0/x/hm/build/hm/hm
```

# Language Features (Types & Operators)
Most Features of the Type System and Operators come from R5RS (https://schemers.org/Documents/Standards/R5RS/HTML/) of Scheme, the standard for Scheme like Languages.

## Type rules of R5RS Scheme
![Scheme Type Rules](https://raw.githubusercontent.com/seanwestfall/hm/master/img/scheme_type_rules.png)
![Operational Semantics](https://raw.githubusercontent.com/seanwestfall/hm/master/img/operational_semantics.png)

## Type System
* An `Atom`, which stores a String naming the atom
* A `List`, which stores a list of other `LispVals` (Haskell lists are denoted by brackets); also called a `proper` list
* A `DottedList`, representing the `Scheme` form `(a b . c)`; also called an `improper` list. This stores a list of all elements but the last, and then stores the last element as another field
* A `Number`, containing a Haskell Integer
* A `String`, containing a Haskell String
* A `Bool`, containing a Haskell boolean value

## Primitive Operations
* `+` plus/addition operator
* `-` minus/subtraction operator
* `*` times/multiplication operator
* `/` divide operator
* `mod` modulus operator
* `quotient` quotient operator
* `remainder` remainder operator / returns the remainder

## Conditionals

## List Operations

## Functions

```
./hm
hm>>> (define (f x y) (+ x y))
(lambda ("x" "y") ...)
hm>>> (f 1 2)
3
```

# imgs
![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_1.png)
![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_2.png)
![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_2_2.png)
![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_3.png)
<!-- ![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_9.png) -->
![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_4.png)
![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_5.png)
![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_6.png)
![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_7.png)
![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_8.png)
