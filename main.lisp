(ql:quickload '(:cl-charms :cl-setlocale))

(defpackage :main
  (:use :cl :charms :charms/ll :cl-setlocale)
  (:shadow :curses-version))
(in-package :main)

(defun main ()
  (set-all-to-native)
  (with-curses ()
    (let* ((a "K1Я1+aИkawa11ya++a-")
           (b 19)
           (c)
           (d)
           (e *standard-window*)
           (f (charms::window-pointer e)))
      (disable-echoing)
      (enable-raw-input)
      (enable-non-blocking-mode e)

      (start-color)
      (dotimes (i b)
        (init-color (+ 8 i) 0 (* (floor 1000 b) (- b i)) 0)
        (init-pair (1+ i) (+ 8 i) 0))

      (push '(0 0 1 0) c)

      (loop
        (multiple-value-bind (width height)
            (window-dimensions e)
          (clear-window e)

          (dolist (char c)
            (destructuring-bind (x y color-pair-id char-idx) char
              (let ((cp (color-pair color-pair-id)))
                (wattron f cp)
                (write-string-at-point e (subseq a char-idx (1+ char-idx)) x y)
                (wattroff f cp))
              (if (< (1+ y) height)
                  (push (list x (1+ y) color-pair-id (mod (1+ char-idx) b)) d))
              (if (and (= y 0) (< color-pair-id b))
                  (push (list x 0 (1+ color-pair-id) char-idx) d))))
          (if (= (random 2) 0)
              (push (list (random (1- width)) 0 1 (random b)) d))
          (setf c d d nil)

          (refresh-window e)
          
          (case (get-char e :ignore-error t)
            ((#\q #\Q) (return)))
          (sleep 0.1))))))


