![](https://github.com/seanwestfall/hm/blob/master/img/hm.png)
# hm
An implementation of the Hindley-Milner type system ([wikipedia](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)) in a simple Scheme/Lisp like language.

# build
```bash
# ghc -o dist/hm --make src/hs.hs
# ./dist/hm ...
```

# Type System
* An `Atom`, which stores a String naming the atom
* A `List`, which stores a list of other `LispVals` (Haskell lists are denoted by brackets); also called a `proper` list
* A `DottedList`, representing the `Scheme` form `(a b . c)`; also called an `improper` list. This stores a list of all elements but the last, and then stores the last element as another field
* A `Number`, containing a Haskell Integer
* A `String`, containing a Haskell String
* A `Bool`, containing a Haskell boolean value

# Primitive Operations
* + plus/addition operator
* - minus/subtraction operator
* * times/multiplication operator
* / divide operator
* mod modulus operator
* quotient quotient operator
* remainder remainder operator / returns the remainder

# imgs
![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_1.png)
![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_2.png)
![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_3.png)
![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_9.png)
![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_4.png)
![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_5.png)
![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_6.png)
![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_7.png)
![](https://raw.githubusercontent.com/seanwestfall/hm/master/img/img_8.png)
