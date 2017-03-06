(in-package :cl-user)
(defpackage cl-chess
  (:use :cl)
  (:export :make-pos
           :make-pos-by-intl
           :pos-row
           :pos-col
           :make-chess-figure
           :figure-pos
           :figure-color
           :figure-type)
  (:import-from :cl-chess.utils
                :is-member
                :build-mapping
                :mapping-value
                :stepping))

(in-package :cl-chess)


;; Приращения, соответствующие сторонам света
(defconstant +N+  #(-1 0))
(defconstant +NE+ #(-1 1))
(defconstant +E+  #(0 1))
(defconstant +SE+ #(1 1))
(defconstant +S+  #(1 0))
(defconstant +SW+ #(1 -1))
(defconstant +W+  #(0 -1))
(defconstant +NW+ #(-1 -1))

(defconstant +direction+ (list +N+ +NE+ +E+ +SE+
                              +S+ +SW+ +W+ +NW+))


;; Цвета фигур
(defconstant +black+ 0)
(defconstant +white+ 1)

(defconstant +figure-color+ (list +black+ +white+))


;; Типы фигур
(defconstant +pawn+ 0)
(defconstant +bishop+ 1)
(defconstant +knight+ 2)
(defconstant +rook+ 3)
(defconstant +queen+ 4)
(defconstant +king+ 5)

(defconstant +figure-type+ (list +pawn+ +bishop+
                                 +knight+ +rook+
                                 +queen+ +king+))

(defconstant +board-size+ 8)

(defconstant +chess-col-&-inx-map+ (build-mapping
                                    '("a" "b" "c" "d" "e" "f" "g" "h")
                                    '(0 1 2 3 4 5 6 7))
  "Конвертирует имена столбцов шахматной доски и их индекс")


;;; Представляет координаты позиции шахматной фигуры
(defun make-pos (row-inx col-inx)
  (vector row-inx col-inx))

(defun make-pos-by-intl (intl-val)
  (make-pos
   (- +board-size+
      (parse-integer (subseq intl-val 1 2)))
   (mapping-value (subseq intl-val 0 1)
                  +chess-col-&-inx-map+)))

(defun pos-row (pos)
  (aref pos 0))

(defun pos-col (pos)
  (aref pos 1))

(defun intl-pos (pos)
  "Позиция в международных обозначениях"
  (concatenate 'string
               (mapping-value (pos-col pos) +chess-col-&-inx-map+)
               (write-to-string (- +board-size+ (pos-row pos)))))

(defun on-board (pos)
  "Указанная позиция находится внутри шахматной доски?"
  (let ((positions (list (pos-row pos) (pos-col pos))))
    (every (lambda (x) (and (>= x 0)
                            (< x +board-size+)))
           positions)))

;;; Описывает шахматную фигуру. Ее параметры: цвет, тип фигуры и координаты на
;;; шахматной доске
(defun make-chess-figure (color type pos)
  (if (on-board pos)
      (vector color type pos)
      nil))

(defun figure-color (figure)
  (aref figure 0))

(defun figure-type (figure)
  (aref figure 1))

(defun figure-pos (figure)
  (aref figure 2))


;;; Вспомогательные функции
(defun direction-stepper (direction)
  (lambda (pos)
     (make-pos (+ (pos-row pos) (aref direction 0))
               (+ (pos-col pos) (aref direction 1)))))



;;; Функции, определяющие ходы типов фигур
(defun pawn-turns (pos)
  (remove-if-not #'on-board
                 (reduce (lambda (acc x)
                           (nconc acc (stepping (direction-stepper x) pos 1)))
                         (list +NE+ +N+ +NW+)
                         :initial-value '())))

(defun bishop-turns (pos &optional (depth +board-size+))
  (remove-if-not
   #'on-board
   (reduce (lambda (acc x)
             (nconc acc (stepping (direction-stepper x) pos depth)))
           (list +NE+ +SE+ +SW+ +NW+)
           :initial-value '())))

(defun knight-turns (pos)
  (let ((row (pos-row pos))
        (col (pos-col pos)))
    (remove-if-not #'on-board
                   (list
                    (make-pos (1- row) (- col 2))
                    (make-pos (- row 2) (1- col))
                    (make-pos (- row 2) (1+ col))
                    (make-pos (1- row) (+ col 2))
                    (make-pos (1+ row) (- col 2))
                    (make-pos (+ row 2) (1- col))
                    (make-pos (+ row 2) (1+ col))
                    (make-pos (1+ row) (+ col 2))))))

(defun rook-turns (pos &optional (depth +board-size+))
  (remove-if-not
   #'on-board
   (reduce (lambda (acc x)
             (nconc acc (stepping (direction-stepper x) pos depth)))
           (list +N+ +E+ +S+ +W+)
           :initial-value '())))

(defun queen-turns (pos &optional (depth +board-size+))
  (remove-if-not
   #'on-board
   (nconc (bishop-turns pos depth)
          (rook-turns pos depth))))

(defun king-turns (pos)
  (queen-turns pos 1))
