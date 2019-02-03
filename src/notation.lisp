(defpackage cl-chess.notation
  (:use :cl :smug)
  (:export))

(in-package :cl-chess.notation)


(defpackage cl-chess.notation.utils
  (:use :cl :smug)
  (:export :.one-of
           :.none-of
           :.many
           :.many1
           :token
           :token-id
           :token-val
           :with-token))

(in-package :cl-chess.notation.utils)


(defun .one-of (string)
  (.is (lambda (x) (find x string))))

(defun .none-of (string)
  (.is-not (lambda (x) (find x string))))

(defun .many1 (parser &optional (result-type 'string))
  (.first (.map result-type parser :at-least 1)))

(defun .many (parser &optional (result-type 'string))
  (.first (.map result-type parser :at-least 0)))

(defun token (id val)
  (vector id val))

(defun token-id (token)
  (elt token 0))

(defun token-val (token)
  (elt token 1))

(defmacro with-token (token-id &body forms)
  `(.bind
    (progn ,@forms)
    (lambda (x) (.identity (token ,token-id x)))))

(defpackage cl-chess.notation.fen
  (:use :cl :smug :trivia :cl-chess.notation.utils)
  (:export))

(in-package :cl-chess.notation.fen)
