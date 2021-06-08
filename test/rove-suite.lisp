(defpackage :biguint/test (:use :cl :biguint :rove))
(in-package :biguint/test)

;;; Helper functions.
(defun test-biguint-fun (valset fun
                         &key (res-to-integer nil) (test #'eq))
  "Test a biguint fun over a valset. valset is ((num1 res1) (num2 res2) etc.).
   Every (eq res (fun (make-biguint num))) must be true."
  (ok (notany
       #'null
       (loop
         for (num res) in valset
         collect (funcall test res
                          (if res-to-integer
                              (to-integer (funcall fun (make-biguint num)))
                              (funcall fun (make-biguint num))))))
      (format nil "~A: ok" fun)))

(defun test-biguint-comp (valset fun)
  "Test a biguint comparison.
   valset is a list with elements (result arg1 arg2 arg3). Arguments are
   fed to #'fun in order."
  (ok (notany
       #'null
       (loop for val in valset
             collect (eq (car val)
                         (apply fun (map 'list #'make-biguint (cdr val))))))
      (format nil "~A: ok" fun)))

(defun test-biguint-op (valset fun)
  "Test a biguint operation.
   valset is a list with elements (result arg1 arg2 arg3)."
  (ok (notany
       #'null
       (loop for val in valset
             collect (= (car val)
                        (to-integer
                         (apply fun (map 'list #'make-biguint (cdr val)))))))
      (format nil "~A: ok" fun)))

;;; Tests.
(deftest validation
  (testing "Instantiation"
    (let ((valset (list 0 1 2 100 255 256 342901092899200)))
      (loop for val in valset do
        (make-biguint val))
      (pass "make-biguint: ok")))
  (testing "Readers and tests"
    (test-biguint-fun '((0 T) (1 nil) (32 nil))
                      #'zero-p)
    (test-biguint-fun '((0 0) (3 2) (255 8))
                      #'length-in-bits))
  (testing "Conversions"
    (test-biguint-fun '((32 32) (0 0) (1 1) (256 256))
                      #'to-integer)
    (test-biguint-fun '((0 #*0) (1 #*1) (2 #*01) (255 #*11111111)
                        (256 #*000000001))
                      #'to-bitvector :test #'equal))
  (testing "Comparisons"
    (test-biguint-comp '((T 1 3) (T 0 1) (T 0 1 2 3) (nil 0 0) (nil 89 89))
                       #'lt)
    (test-biguint-comp '((T 3 0) (nil 0 3) (T 4500 0) (T 3 2 1))
                       #'gt)
    (test-biguint-comp '((T 1 3) (T 0 1) (T 0 0) (T 9000 445000)
                         (nil 90 1045 34 7 8) (T 0 0 0 0))
                       #'leq)
    (test-biguint-comp '((T 0 0 0 0) (nil 590 8029292) (nil 4 2 1 292920))
                       #'geq)
    (test-biguint-comp '((T 0 0 0 0) (T 0 0) (T 1 1) (T 89 89 89) (T 255 255)
                         (T 256 256) (T 45 45 45 45 45))
                       #'b=))
  (testing "Operations"
    (test-biguint-op '((9 3 6) (0 0 0) (0 0) (0 0 0 0) (1 0 1)
                       (1 1) (0 0) (90 45 30 15))
                     #'add)
    (test-biguint-op '((0 0 0 0) (132 145 10 3))
                     #'sub)
    (test-biguint-op '((0 0) (59 59) (1 1 1) (90 3 30) (130 1 10 13))
                     #'mul)))
