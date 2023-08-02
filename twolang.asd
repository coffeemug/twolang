(defsystem :twolang
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:twolang/main)
  :in-order-to ((test-op (test-op :twolang/test))))

(defsystem :twolang/test
  :class :package-inferred-system
  :depends-on (:twolang/test/basics)
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :basic-test-suite :twolang/test/basics))))

(register-system-packages :maxpc '(:maxpc.char :maxpc.digit :maxpc.input))
