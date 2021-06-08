(defpackage :biguint
  (:use :cl)
  (:export
   ;; Instantiation
   #:make-biguint
   ;; Readers and tests
   #:zero-p
   #:length-in-bits
   ;; Conversions
   #:to-integer
   #:to-bitvector
   ;; Comparisons
   #:lt
   #:gt
   #:leq
   #:geq
   #:b=
   ;; Operations
   #:add
   #:sub
   #:mul))
