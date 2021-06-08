(defsystem biguint
  :name "biguint"
  :author "Thomas HOULLIER"
  :depends-on ("arbprec")
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "representation" :depends-on ("package"))
                 (:file "compare" :depends-on ("representation")))))
  :in-order-to ((test-op (test-op "biguint/test"))))

(defsystem biguint/test
  :name "biguint/test"
  :depends-on ("rove" "biguint")
  :components
  ((:module "test"
    :components ((:file "rove-suite"))))
  :perform (test-op (o c) (symbol-call :rove '#:run-suite :biguint/test)))
