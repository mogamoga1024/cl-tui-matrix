(ql:quickload '(:cl-charms :cl-setlocale))
(defpackage :main (:use :cl :charms :charms/ll :cl-setlocale)
(:shadow :curses-version))(in-package :main)

(defun main ()
  (set-all-to-native)
  (with-curses ()
    (let* ((kiri-kawa "K1Я1+aИkawa11ya++a-")
           (kiri-kawa-len (length kiri-kawa))
           (char-list)
           (new-char-list)
           (win *standard-window*)
           (win-ptr (charms::window-pointer win)))
      (disable-echoing)
      (enable-raw-input)
      (enable-non-blocking-mode win)

      (start-color)
      (dotimes (i kiri-kawa-len)
        (init-color (+ 8 i) 0 (* (floor 1000 kiri-kawa-len) (- kiri-kawa-len i)) 0)
        (init-pair (1+ i) (+ 8 i) 0))

      (loop
        (multiple-value-bind (width height)
            (window-dimensions win)
          (clear-window win)

          (dolist (char char-list)
            (destructuring-bind (x y color-pair-id char-idx) char
              (wattron win-ptr (color-pair color-pair-id))
              (write-string-at-point win (subseq kiri-kawa char-idx (1+ char-idx)) x y)
              (wattroff win-ptr (color-pair color-pair-id))
              (when (< (1+ y) height)
                (push (list x (1+ y) color-pair-id (mod (1+ char-idx) kiri-kawa-len)) new-char-list))
              (when (and (= y 0) (< color-pair-id kiri-kawa-len))
                (push (list x 0 (1+ color-pair-id) char-idx) new-char-list))))
          (let ((new-x (random (1- width))))
            (when (every #'(lambda (lst) (or (/= (car lst) new-x) (/= (cadr lst) 0))) new-char-list)
              (push (list new-x 0 1 (random kiri-kawa-len)) new-char-list)))
          (setf char-list new-char-list new-char-list nil)

          (refresh-window win)
          
          (case (get-char win :ignore-error t)
            ((#\q #\Q) (return)))
          (sleep 0.1))))))


