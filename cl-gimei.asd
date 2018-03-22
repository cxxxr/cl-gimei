(defsystem "cl-gimei"
  :class :package-inferred-system
  :license "MIT"
  :author "cxxxr"
  :description "random japanese name and address generator"
  :depends-on ("cl-gimei/main")
  :in-order-to ((test-op (test-op "cl-gimei/test"))))

(defsystem "cl-gimei/test"
  :class :package-inferred-system
  :depends-on ("rove" "cl-gimei/test")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
