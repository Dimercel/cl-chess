(defpackage cl-chess/tests/main
  (:use :cl
        :cl-chess
        :rove))
(in-package :cl-chess/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-chess)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
