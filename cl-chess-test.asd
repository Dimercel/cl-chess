#|
  This file is a part of cl-chess project.
  Copyright (c) 2017 Ito Dimercel (xolcman@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-chess-test-asd
  (:use :cl :asdf))
(in-package :cl-chess-test-asd)

(defsystem cl-chess-test
  :author "Ito Dimercel"
  :license "MIT"
  :depends-on (:cl-chess
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-chess"))))
  :description "Test system for cl-chess"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
