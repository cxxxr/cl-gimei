(defsystem "cl-gimei"
  :class :package-inferred-system
  :license "MIT"
  :author "cxxxr"
  :description "random japanese name and address generator"
  :depends-on ("cl-gimei/main"))

(defsystem "cl-gimei/test"
  :class :package-inferred-system
  :depends-on ("cl-gimei/test")
  :perform (test-op (o c) (symbol-call :prove '#:run :cl-gimei)))
