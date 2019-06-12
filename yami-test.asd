#|
  This file is a part of yami project.
  Copyright (c) 2019 carrotflakes (carrotflakes@gmail.com)
|#

(defsystem "yami-test"
  :defsystem-depends-on ("prove-asdf")
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on ("yami"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "yami"))))
  :description "Test system for yami"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
