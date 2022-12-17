(ql:quickload '(:cl-charms :cl-setlocale))

(defpackage :main
  (:use :cl :charms :charms/ll :cl-setlocale)
  (:shadow :curses-version))
(in-package :main)

(defun main ()
  (set-all-to-native)
  (with-curses ()
    (let* ((kiri-kawa "K1Я1+aИkawa11ya++a-")
           (kiri-kawa-len 19)
           (char-list)
           (new-char-list)
           (win *standard-window*)
           (winp (charms::window-pointer win)))
      (disable-echoing)
      (enable-raw-input)
      (enable-non-blocking-mode win)

      (start-color)
      (dotimes (i kiri-kawa-len)
        (init-color (+ 8 i) 0 (* (floor 1000 kiri-kawa-len) (- kiri-kawa-len i)) 0)
        (init-pair (1+ i) (+ 8 i) 0))

      (push '(0 0 1 0) char-list)

      (loop
        (multiple-value-bind (width height)
            (window-dimensions win)
          (clear-window win)

          (dolist (char char-list)
            (destructuring-bind (x y color-pair-id char-idx) char
              (let ((cp (color-pair color-pair-id)))
                (wattron winp cp)
                ;; xが(1- width)かつyが(1- height)だと落ちる
                (write-string-at-point win (subseq kiri-kawa char-idx (1+ char-idx)) x y)
                (wattroff winp cp))
              ;; 一段下げる
              (if (< (1+ y) height)
                  (push (list x (1+ y) color-pair-id (mod (1+ char-idx) kiri-kawa-len)) new-char-list))
              (if (and (= y 0) (< color-pair-id kiri-kawa-len))
                  (push (list x 0 (1+ color-pair-id) char-idx) new-char-list))))
          ;; 新しいやつ生成
          (if (= (random 2) 0)
              (push (list (random (1- width)) 0 1 (random kiri-kawa-len)) new-char-list))
          (setf char-list new-char-list new-char-list nil)

          (refresh-window win)
          
          (case (get-char win :ignore-error t)
            ((#\q #\Q) (return)))
          (sleep 0.1))))))


