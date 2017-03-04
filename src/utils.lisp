(in-package :cl-user)
(defpackage cl-chess.utils
  (:use :cl)
  (:export :is-member
           :build-mapping
           :mapping-value
           :stepping))

(in-package :cl-chess.utils)


(defun is-member (item list)
  "Присутствует ли item в указанном списке"
  (not (null (member item list))))

(defun build-mapping (values1 values2)
  "Строит взаимнооднозначное отображение"
  (append (mapcar #'cons values1 values2)
          (mapcar #'cons values2 values1)))

(defun mapping-value (key mapping)
  "Вернет значение соотв. key из отображения"
  (cdr (assoc key mapping :test 'equal)))

(defun stepping (stepper value step-count)
  "Применяет stepper к value step-count раз.
   Возвращает список из получившихся значений"
  (if (= 0 step-count)
      '()
      (let ((new-value (funcall stepper value)))
        (cons new-value (stepping stepper
                                  new-value
                                  (1- step-count))))))
