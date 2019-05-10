(defpackage cl-chess.notation
  (:use :cl :smug)
  (:export))

(in-package :cl-chess.notation)


(defpackage cl-chess.notation.utils
  (:use :cl :smug)
  (:export :char2digit
           :.one-of
           :.none-of
           :.many
           :.many1
           :.count
           :token
           :token-id
           :token-val
           :with-token))

(in-package :cl-chess.notation.utils)


(defun char2digit (char)
  (- (char-code char) 48))

(defun .one-of (string)
  (.is (lambda (x) (find x string))))

(defun .none-of (string)
  (.is-not (lambda (x) (find x string))))

(defun .many1 (parser &optional (result-type 'string))
  (.first (.map result-type parser :at-least 1)))

(defun .many (parser &optional (result-type 'string))
  (.first (.map result-type parser :at-least 0)))

(defmacro .count (result-type count parser)
  (if (not (= count 0))
      (let ((name (gensym))
            (other (gensym)))
        `(.let* ((,name ,parser)
                 (,other (.count ,result-type ,(1- count) ,parser)))
           (.identity (concatenate ,result-type ,name ,other))))
      `(.identity nil)))

(defstruct token
  id
  val)

(defmacro with-token (token-id action &body forms)
  (let ((fn (if (null action) 'identity action)))
    `(.bind
      (progn ,@forms)
      (lambda (x) (.identity (make-token :id ,token-id :val (funcall ,fn x)))))))

(defpackage cl-chess.notation.fen
  (:use :cl :smug :trivia :cl-chess.notation.utils)
  (:export))

(in-package :cl-chess.notation.fen)

(defun .ws ()
  (.string= " "))

(defun .figure ()
  (.let* ((figure (.one-of "kqrbnpKQRBNP")))
    (.identity
     (case figure
       (#\k (make-token :id :black-king   :val figure))
       (#\q (make-token :id :black-queen  :val figure))
       (#\r (make-token :id :black-rook   :val figure))
       (#\b (make-token :id :black-bishop :val figure))
       (#\n (make-token :id :black-knight :val figure))
       (#\p (make-token :id :black-pawn   :val figure))
       (#\K (make-token :id :white-king   :val figure))
       (#\Q (make-token :id :white-queen  :val figure))
       (#\R (make-token :id :white-rook   :val figure))
       (#\B (make-token :id :white-bishop :val figure))
       (#\N (make-token :id :white-knight :val figure))
       (#\P (make-token :id :white-pawn   :val figure))))))

(defun .space ()
  (with-token :space 'char2digit
    (.one-of "12345678")))

(defun .separator ()
  (with-token :separator nil
    (.char= #\/)))

(defun .side-move ()
  (with-token :side nil
    (.one-of "bw")))

(defun .castle ()
  (with-token :castle nil
    (.or (.string= "-")
         (.many1 (.one-of "kqKQ-")))))

(defun .two-square ()
  (with-token :two-square nil
    (.or (.string= "-")
         (.many1 (.one-of "abcdefgh12345678")))))

(defun .halfmove ()
  (with-token :halfmove 'parse-integer
    (.many1 (.is 'digit-char-p))))

(defun .fullmove ()
  (with-token :fullmove 'parse-integer
    (.many1 (.is 'digit-char-p))))

(defun .row ()
  (with-token :row nil
    (.prog1
     (.many1 (.or (.figure)
                  (.space))
             'list)
     (.plus (.separator)
            (.ws)))))

(defun .fen ()
  (.let* ((state (.count 'list 8 (.row)))
          (side (.side-move))
          (_ (.ws))
          (castle (.castle))
          (_ (.ws))
          (two-square (.two-square))
          (_ (.ws))
          (halfmove (.halfmove))
          (_ (.ws))
          (fullmove (.fullmove)))
    (.identity (list state side castle two-square halfmove fullmove))))
