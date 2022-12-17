
(ql:quickload '(:cl-charms :cl-setlocale))

(defpackage :main
  (:use :cl :charms :cl-charms))
(in-package :main)

(defun main ()
  (cl-setlocale:set-all-to-native)
  (let ((kiri-kawa "K1Я1+aИkawa11ya++a-")
        (kiri-kawa-len 19)
        (char-list)
        (new-char-list))
    (charms:with-curses ()
      (charms:disable-echoing)
      (charms:enable-raw-input)
      (charms:enable-non-blocking-mode charms:*standard-window*)

      (charms/ll:start-color)
      (dotimes (i kiri-kawa-len)
        (charms/ll:init-color (+ 8 i) 0 (* (floor 1000 kiri-kawa-len) (- kiri-kawa-len i)) 0)
        (charms/ll:init-pair (1+ i) (+ 8 i) charms/ll:COLOR_BLACK))

      (push '(0 0 1 0) char-list)

      (loop
        (multiple-value-bind (width height)
            (charms:window-dimensions charms:*standard-window*)
          (charms:clear-window charms:*standard-window*)

          (dolist (char char-list)
            (destructuring-bind (x y color-pair-id char-idx) char
              (charms/ll:wattron (charms::window-pointer charms:*standard-window*) (charms/ll:color-pair color-pair-id))
              ;; xが(1- width)かつyが(1- height)だと落ちる
              (charms:write-string-at-point charms:*standard-window* (subseq kiri-kawa char-idx (1+ char-idx)) x y)
              (charms/ll:wattroff (charms::window-pointer charms:*standard-window*) (charms/ll:color-pair color-pair-id))
              ;; 一段下げる
              (if (< (1+ y) height)
                  (push (list x (1+ y) color-pair-id (mod (1+ char-idx) kiri-kawa-len)) new-char-list))
              (if (and (= y 0) (< color-pair-id kiri-kawa-len))
                  (push (list x 0 (1+ color-pair-id) char-idx) new-char-list))))
          ;; 新しいやつ生成
          (if (= (random 2) 0)
              (push (list (random (1- width)) 0 1 (random kiri-kawa-len)) new-char-list))
          (setf char-list new-char-list new-char-list nil)

          (charms:refresh-window charms:*standard-window*)
          
          (case (charms:get-char charms:*standard-window* :ignore-error t)
            ((#\q #\Q) (return)))
          (sleep 0.1))))))


