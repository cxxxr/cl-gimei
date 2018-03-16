(defsystem "cl-gimei-test"
  :depends-on ("cl-gimei" "prove")
  :serial t
  :components ((:file "test"))
  :perform (test-op (o c) (symbol-call :prove '#:run :cl-gimei)))
