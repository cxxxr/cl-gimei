(defsystem "cl-gimei/test"
  :class :package-inferred-system
  :depends-on ("cl-gimei/test")
  :perform (test-op (o c) (symbol-call :prove '#:run :cl-gimei)))
