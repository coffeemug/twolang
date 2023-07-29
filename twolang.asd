(defsystem :twolang
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:twolang/main))

(register-system-packages :maxpc '(:maxpc.char :maxpc.digit))
