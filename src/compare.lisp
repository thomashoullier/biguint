;;;; Comparisons for biguints
(in-package :biguint)

;;; Binary versions to be generalized at the top-level.
(defmethod binary-less-than ((biguint1 biguint) (biguint2 biguint))
  "Binary < operator for biguint."
  (case (arbprec:compare-words (words biguint1) (words biguint2))
    (arbprec:less-than T)
    (T nil)))

(defmethod binary-greater-than ((biguint1 biguint) (biguint2 biguint))
  (case (arbprec:compare-words (words biguint1) (words biguint2))
    (arbprec:greater-than T)
    (T nil)))

(defmethod binary-leq ((biguint1 biguint) (biguint2 biguint))
  (case (arbprec:compare-words (words biguint1) (words biguint2))
    ((arbprec:less-than arbprec:same) T)
    (T nil)))

(defmethod binary-geq ((biguint1 biguint) (biguint2 biguint))
  (case (arbprec:compare-words (words biguint1) (words biguint2))
    ((arbprec:greater-than arbprec:same) ' T)
    (T nil)))

(defmethod binary-equality ((biguint1 biguint) (biguint2 biguint))
  "Binary equality operator for biguints."
  (eq (arbprec:compare-words (words biguint1) (words biguint2)) 'arbprec:same))

;;; Comparisons
(defun lt (&rest biguints) (map-reduce-over-pairs #'binary-less-than biguints))
(defun gt (&rest biguints) (map-reduce-over-pairs
                            #'binary-greater-than biguints))
(defun leq (&rest biguints) (map-reduce-over-pairs #'binary-leq biguints))
(defun geq (&rest biguints) (map-reduce-over-pairs #'binary-geq biguints))
(defun b= (&rest biguints) (map-reduce-over-pairs #'binary-equality biguints))

;;; Comparison helper (internal)
(defun map-reduce-over-pairs (fun list)
  "Map a test of (x y) onto successive pairs in a list.
   Then AND all entries."
  (loop for (x y) on list while y do
    (when (not (funcall fun x y)) (return-from map-reduce-over-pairs nil)))
  T)
