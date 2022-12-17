
(ql:quickload '(:cl-charms :cl-setlocale))

(defpackage :main
  (:use :cl))
(in-package :main)

(defun main ()
  (cl-setlocale:set-all-to-native)
  (charms:with-curses ()
    (let* ((kiri-kawa "K1Я1+aИkawa11ya++a-")
           (kiri-kawa-len (length kiri-kawa))
           (char-list)
           (new-char-list)
           (win charms:*standard-window*)
           (win-ptr (charms::window-pointer win)))
      (charms:disable-echoing)
      (charms:enable-raw-input)
      (charms:enable-non-blocking-mode win)

      (charms/ll:start-color)
      (dotimes (i kiri-kawa-len)
        (charms/ll:init-color (+ 8 i) 0 (* (floor 1000 kiri-kawa-len) (- kiri-kawa-len i)) 0)
        (charms/ll:init-pair (1+ i) (+ 8 i) charms/ll:COLOR_BLACK))

      (loop
        (multiple-value-bind (width height)
            (charms:window-dimensions win)
          (charms:clear-window win)

          (dolist (char char-list)
            (destructuring-bind (x y color-pair-id char-idx) char
              (let ((color-pair (charms/ll:color-pair color-pair-id)))
                (charms/ll:wattron win-ptr color-pair)
                ;; xが(1- width)かつyが(1- height)だと落ちる
                (charms:write-string-at-point win (subseq kiri-kawa char-idx (1+ char-idx)) x y)
                (charms/ll:wattroff win-ptr color-pair))
              ;; 一段下げる
              (when (< (1+ y) height)
                (push (list x (1+ y) color-pair-id (mod (1+ char-idx) kiri-kawa-len)) new-char-list))
              (when (and (= y 0) (< color-pair-id kiri-kawa-len))
                (push (list x 0 (1+ color-pair-id) char-idx) new-char-list))))
          ;; 新しいやつ生成
          (let ((new-x (random (1- width))))
            (when (every #'(lambda (lst) (or (/= (car lst) new-x) (/= (cadr lst) 0))) new-char-list)
              (push (list new-x 0 1 (random kiri-kawa-len)) new-char-list)))
          (setf char-list new-char-list new-char-list nil)

          (charms:refresh-window win)
          
          (case (charms:get-char win :ignore-error t)
            ((#\q #\Q) (return)))
          (sleep 0.1))))))


