;;;; biguint class
(in-package :biguint)

(defclass biguint ()
  ((words :documentation "Vector of bytes storing the number."
          :accessor words :initarg :words)))

;;; Instantiation
(defun make-biguint (integer)
  "Instantiate a biguint from a native integer (>= 0)."
  (make-instance 'biguint :words (arbprec:make-words integer)))

(defun make-biguint-from-words (words)
  "Create a new biguint from already existing words. Internal"
  (make-instance 'biguint :words words))

;;; Readers and tests.
(defmethod zero-p ((biguint biguint))
  "Is the biguint zero?"
  (arbprec:zero-p (words biguint)))

(defmethod length-in-bits ((biguint biguint))
  "Length of a biguint in bits. Up to the last non-zero MSB."
  (arbprec:length-in-bits (words biguint)))

;;; Conversions
(defmethod to-integer ((biguint biguint))
  "Convert biguint to a native integer."
  (arbprec:to-integer (words biguint)))

(defmethod to-bitvector ((biguint biguint))
  "Convert biguint to bitvector. LSB is first. Up to last non-zero MSB."
  (arbprec:to-bitvector (words biguint)))

;;; +, -, *
(defun add (&rest biguints)
  "Addition operator for biguints."
  (make-biguint-from-words (reduce #'arbprec:add (map 'list #'words biguints))))

(defun sub (&rest biguints)
  "Subtraction operator for biguints."
  (make-biguint-from-words (reduce #'arbprec:sub (map 'list #'words biguints))))

(defun mul (&rest biguints)
  "Multiplication operator for biguints."
  (make-biguint-from-words (reduce #'arbprec:mul (map 'list #'words biguints))))
