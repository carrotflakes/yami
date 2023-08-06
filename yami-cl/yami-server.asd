#|
  This file is a part of yami project.
  Copyright (c) 2019 carrotflakes (carrotflakes@gmail.com)
|#

#|
  Author: carrotflakes (carrotflakes@gmail.com)
|#

(defsystem "yami-server"
  :version "0.1.0"
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on ("ningle"
               "clack"
               "lack-middleware-static"
               "yami")
  :components ((:module "server"
                :components
                ((:file "server"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  ;:in-order-to ((test-op (test-op "yami-test")))
  )
