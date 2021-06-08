# Biguint
`biguint` implements a big unsigned integer representation and
arithmetic for Common Lisp. It is built on top of
[arbprec](https://github.com/thomashoullier/arbprec) (see for more
context and references).

## Usage
We defined a `biguint` class that is simply a (internal) vector
of bytes that stores an arbitrary precision integer with corresponding
methods which we list here.

### Instantiation
**make-biguint** integer => *biguint*

Create a `biguint` from a native `integer` (can be a `bignum`).

```common-lisp
(make-biguint 90)
;;=> #<BIGUINT {XXXXXXXXXX}>
```

### Readers and tests
**zero-p** biguint => *boolean*

Test whether `biguint` is zero.

```common-lisp
(zero-p (make-biguint 0))
;;=> T
(zero-p (make-biguint 43))
;;=> nil
```

**length-in-bits** biguint => *len*

Return the number of bits used to represent `biguint` up to the last non-zero
MSB. Zero has a length of 0 bit.

```common-lisp
(length-in-bits (make-biguint 4))
;;=> 3
(length-in-bits (make-biguint 256))
;;=> 9
(length-in-bits (make-biguint 0))
;;=> 0
```

### Conversions
**to-integer** biguint => *integer*

Convert a `biguint` back to a native integer (including bignums).

```common-lisp
(to-integer (make-biguint 8))
;; => 8
```

**to-bitvector** biguint => *bitvector*

Show a bitvector representation of `biguint`. LSB to MSB from left to right.
The last bit is always 1, except for zero.

```common-lisp
(to-bitvector (make-biguint 255))
;;=> #*11111111
(to-bitvector (make-biguint 0))
;;=> #*0
```

### Comparisons
**lt** *&rest* biguints => *boolean*
**gt** *&rest* biguints => *boolean*
**leq** *&rest* biguints => *boolean*
**geq** *&rest* biguints => *boolean*

* **lt**: less than
* **gt**: greater than
* **leq**: less than or equal
* **geq**: greater than or equal

All work in the same fashion as their native counterparts `<`, `>`, `<=`, `>=`.

```common-lisp
(lt (make-biguint 80) (make-biguint 900))
;;=> T
(geq (make-biguint 0) (make-biguint 7) (make-biguint 4))
;;=> nil
```

**b=** *&rest* biguints => *boolean*

Equality predicate for `biguint`.

```common-lisp
(b= (make-biguint 93) (make-biguint 93))
;;=> T
(b= (make-biguint 4) (make-biguint 43))
;;=> nil
```

### Operations
**add** *&rest* biguints => *added-biguints*

Addition over biguints.

```common-lisp
(to-integer (add (make-biguint 3) (make-biguint 5)))
;;=> 8
```

**sub** *&rest* biguints => *subtracted-biguint*

Subtraction over biguints. Undefined behaviour if the result is negative.

```common-lisp
(to-integer (sub (make-biguint 14) (make-biguint 8) (make-biguint 3)))
;;=> 3
```

**mul** *&rest* biguints => *product-biguint*

Multiplication over biguints.

```common-lisp
(to-integer (mul (make-biguint 8) (make-biguint 2)))
;;=> 16
```

## Dependencies
* `biguint`: [arbprec](https://github.com/thomashoullier/arbprec)
* `biguint/test`:
  * [rove](https://github.com/fukamachi/rove)

## Tests
Launch tests with:

```common-lisp
(asdf:test-system "biguint")
```
