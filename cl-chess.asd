#|
  This file is a part of cl-chess project.
  Copyright (c) 2017 Ito Dimercel (xolcman@gmail.com)
|#

#|
  Author: Ito Dimercel (xolcman@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-chess-asd
  (:use :cl :asdf))
(in-package :cl-chess-asd)

(defsystem cl-chess
  :version "0.1"
  :author "Ito Dimercel"
  :license "MIT"
  :depends-on (:alexandria :smug)
  :components ((:module "src"
                :components
                ((:file "cl-chess" :depends-on ("utils"))
                 (:file "notation" :depends-on ("utils" "cl-chess"))
                 (:file "utils"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-chess-test))))
