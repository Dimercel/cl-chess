(defsystem "cl-chess"
  :long-name "Chess library"
  :version "0.1"
  :author "Ito Dimercel"
  :maintainer "Ito Dimercel"
  :mailto "xolcman@gmail.com"
  :license "MIT"
  :homepage "https://github.com/Dimercel/cl-chess"
  :bug-tracker "https://github.com/Dimercel/cl-chess/issues"
  :source-control "https://github.com/Dimercel/cl-chess.git"
  :depends-on ("alexandria"
               "smug"
               "trivia")
  :components ((:module "src"
                :components
                ((:file "cl-chess" :depends-on ("utils"))
                 (:file "notation" :depends-on ("utils" "cl-chess"))
                 (:file "utils"))))
  :description "Chesslibrary"
  :long-description "Chess library"
  :in-order-to ((test-op (test-op "cl-chess/tests"))))

(defsystem "cl-chess/tests"
  :author "Ito Dimercel"
  :license "MIT"
  :depends-on ("cl-chess"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-chess"
  :perform (test-op (op c) (symbol-call :rove :run c)))
