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
  (:import-from :alexandria
                :define-constant)
  (:import-from :cl-chess.utils
                :is-member
                :build-mapping
                :mapping-value
                :stepping))

(in-package :cl-chess)


;; Приращения, соответствующие сторонам света
(define-constant +N+  #(-1 0) :test 'equalp)
(define-constant +NE+ #(-1 1) :test 'equalp)
(define-constant +E+  #(0 1) :test 'equalp)
(define-constant +SE+ #(1 1) :test 'equalp)
(define-constant +S+  #(1 0) :test 'equalp)
(define-constant +SW+ #(1 -1) :test 'equalp)
(define-constant +W+  #(0 -1) :test 'equalp)
(define-constant +NW+ #(-1 -1) :test 'equalp)

(define-constant +direction+ (list +N+ +NE+ +E+ +SE+ +S+ +SW+ +W+ +NW+)
                 :test 'equalp)


;; Цвета фигур
(define-constant +black+ 0 :test 'equalp)
(define-constant +white+ 1 :test 'equalp)

(define-constant +figure-color+ (list +black+ +white+) :test 'equalp)


;; Типы фигур
(define-constant +pawn+ 0 :test 'equalp)
(define-constant +bishop+ 1 :test 'equalp)
(define-constant +knight+ 2 :test 'equalp)
(define-constant +rook+ 3 :test 'equalp)
(define-constant +queen+ 4 :test 'equalp)
(define-constant +king+ 5 :test 'equalp)

(define-constant +figure-type+ (list +pawn+ +bishop+ +knight+ +rook+ +queen+ +king+)
                 :test 'equalp)

(define-constant +board-size+ 8 :test 'equalp)

(define-constant +chess-col-&-inx-map+ (build-mapping
                                        '("a" "b" "c" "d" "e" "f" "g" "h")
                                        '(0 1 2 3 4 5 6 7))
                 :test 'equalp)

(define-constant +figure-type-&-intl+ (build-mapping +figure-type+
                                                     '(#\p #\B #\N #\R #\Q #\K))
                 :test 'equalp)


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

(defun make-chess-figure-by-intl (intl-val color)
  "Создает шахматную фигуру по строке, содержащей
   интернациональные обозначения"
  (make-chess-figure color
                     (mapping-value (char intl-val 0)
                                    +figure-type-&-intl+)
                     (make-pos-by-intl (subseq intl-val 1 3))))

(defun figure-color (figure)
  (aref figure 0))

(defun figure-type (figure)
  (aref figure 1))

(defun figure-pos (figure)
  (aref figure 2))

(defun intl-figure (figure)
  (concatenate 'string
               (list (mapping-value (figure-type figure) +figure-type-&-intl+))
               (intl-pos (figure-pos figure))))


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

(defun starting-board-state ()
  "Генерирует стартовое состояние шахматной доски"
  (nconc
   (map 'list
        (lambda (x) (make-chess-figure-by-intl x +black+))
        (list "Ra8" "Nb8" "Bc8" "Qd8" "Ke8" "Bf8" "Ng8" "Rh8"
              "pa7" "pb7" "pc7" "pd7" "pe7" "pf7" "pg7" "ph7"))
   (map 'list
        (lambda (x) (make-chess-figure-by-intl x +white+))
        (list "pa2" "pb2" "pc2" "pd2" "pe2" "pf2" "pg2" "ph2"
              "Ra1" "Nb1" "Bc1" "Qd1" "Ke1" "Bf1" "Ng1" "Rh1"))))


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

;; МЕТОД КООРДИНАТ.
;;
;;   Шахматная доска имеет размеры 8x8 клеток, каждой из которых соответствует уникальное обозначение
;; состоящие из наименования столбца и строки на пересечении которых находится клетка. Примерами
;; таких обозначений могут быть: f1 - клетка находящаяся в самом низу доски и 6-ом столбце, h4 -
;; находится в самом правом, 8-ом столбце и 4-ой строке. Использование такого метода координат
;; становится очевидным при непосредственном созерцании шахматной доски.
;;   Внутри кода удобней использовать так называемую экранную систему координат, в которой
;; сначало идут координаты строки, а потом столбца. Начало координат находится в левом верхнем углу.
;; В такой системе крайняя левая верхняя точка имеет координаты 0,0.
;;   Тем самым встает вопрос о конвертировании одних кооординат в другие и обратно.
