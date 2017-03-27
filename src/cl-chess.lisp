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

(defun pos-equal (pos1 pos2)
  (and
   (= (pos-row pos1) (pos-row pos2))
   (= (pos-col pos1) (pos-col pos2))))

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

(defun figure-on-pos (pos figures)
  "Возвращает фигуру, стоящую на позиции pos
   или nil, если там нет ни одной фигуры"
  (find-if (lambda (x) (pos-equal (figure-pos x) pos))
           figures))

(defun figure-on-pos-p (pos figures)
  "Стоил ли какая-либо фигура в позиции pos?"
  (if (figure-on-pos pos figures)
      t
      nil))

(defun neighbors-p (pos1 pos2)
  "Вернет истину если позиции клеток pos1 и pos2
   являются соседними. Диагонали не учитываются."
  (= 1
     (+
      (abs (- (pos-row pos1) (pos-row pos2)))
      (abs (- (pos-col pos1) (pos-col pos2))))))

(defun neighbors-diag-p (pos1 pos2)
  "Учитываются диагональные клетки"
  (or
   (neighbors-p pos1 pos2)
   (and
    (= 1 (abs (- (pos-row pos1) (pos-row pos2))))
    (= 1 (abs (- (pos-col pos1) (pos-col pos2)))))))

(defun get-linked (neighbors item coll &key (test 'equal))
  "Вернет список связанных между собой элементов.
   Связанность вычисляется на основе транзитивности
   предиката neighbors"
  (let* ((neighbors-items-p (lambda (x) (funcall neighbors item x)))
         (cur-gen (remove-if-not neighbors-items-p coll))
         (other (remove-if neighbors-items-p coll)))
    (append cur-gen
            (delete-duplicates
             (reduce #'nconc
                     (map 'list
                          (lambda (x)
                            (get-linked neighbors x other :test test))
                          cur-gen))
             :test test))))

(defun invert-color (color)
  (when (= color +black+)
    +white+)
  +black+)

(defun figures-by-color (figures color)
  "Выбирает все фигуры цвета color из списка figures"
  (remove-if-not (lambda (x) (= color (figure-color x))) figures))


;;; Функции, определяющие ходы типов фигур
(defun pawn-turns (pos color)
  (let ((row-inx (pos-row pos))
        (col-inx (pos-col pos))
        (result '()))
    (if (= color +black+)
        (progn
          (setf result
                (list
                 (make-pos (1+ row-inx) col-inx)
                 (make-pos (1+ row-inx) (1- col-inx))
                 (make-pos (1+ row-inx) (1+ col-inx))))
          (when (= 1 row-inx)
            (setf result
                  (cons (make-pos (+ row-inx 2) col-inx) result))))
        (progn
          (setf result
                (list
                 (make-pos (1- row-inx) col-inx)
                 (make-pos (1- row-inx) (1- col-inx))
                 (make-pos (1- row-inx) (1+ col-inx))))
          (when (= 6 row-inx)
            (setf result
                  (cons (make-pos (- row-inx 2) col-inx) result)))))
    (remove-if-not #'on-board result)))

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

(defun get-turns (figure &optional (depth +board-size+))
  "Вычисляет доступные фигуре ходы, без учета остальных."
  (let ((pos (figure-pos figure))
        (type (figure-type figure)))
    (cond
      ((= type +pawn+) (pawn-turns pos (figure-color figure)))
      ((= type +bishop+) (bishop-turns pos depth))
      ((= type +knight+) (knight-turns pos))
      ((= type +rook+) (rook-turns pos depth))
      ((= type +queen+) (queen-turns pos depth))
      ((= type +king+) (king-turns pos))
      (t nil))))

(defun pawn-available-turns (pos figures)
  "Вернет доступные пешке ходы из позиции pos,
   учитывая остальные фигуры на доске figures"
  (let ((pawn (figure-on-pos pos figures))
        (col-inx (pos-col pos)))
    (when (and (not (null pawn))
               (= (figure-type pawn) +pawn+))
      (let* ((turns (pawn-turns pos (figure-color pawn)))
             (attacked (remove-if
                        (lambda (x) (= col-inx (pos-col x))) turns))
             (not-attacked (remove-if-not
                            (lambda (x) (= col-inx (pos-col x))) turns)))
                    (nconc
                     (remove-if-not (lambda (x)
                                      (figure-on-pos x figures))
                                    attacked)
                     (get-linked 'neighbors-diag-p
                                 pos
                                 (remove-if (lambda (x)
                                              (figure-on-pos x figures))
                                            not-attacked)
                                 :test 'pos-equal))))))
