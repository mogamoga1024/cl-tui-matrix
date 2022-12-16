
(ql:quickload '(:cl-charms :cl-setlocale))

(defpackage :main
  (:use :cl :charms :cl-charms))
(in-package :main)

(defparameter *kiri-kawa* "ｰﾀｯﾔｲｲﾜｶﾝﾀﾘｷ")
(defconstant +kiri-kawa-len+ (length *kiri-kawa*))
(defparameter *char-list* nil)
(defparameter *new-char-list* nil)

(defun main ()
  (cl-setlocale:set-all-to-native)

  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input)
    (charms:enable-non-blocking-mode charms:*standard-window*)

    (charms/ll:start-color)
    (dotimes (i +kiri-kawa-len+)
      (charms/ll:init-color (+ 8 i) 0 (* (floor 1000 +kiri-kawa-len+) (- +kiri-kawa-len+ i)) 0)
      (charms/ll:init-pair (1+ i) (+ 8 i) charms/ll:COLOR_BLACK))

    ; x y color-pair-id char-idx
    (push '(0 0 1 0) *char-list*)
    ; (push '(0 1 1 0) *char-list*)
    ; (push '(0 0 2 1) *char-list*)

    (loop
      (charms:clear-window charms:*standard-window*)

      (dolist (char *char-list*)
        (destructuring-bind (x y color-pair-id char-idx) char
          (charms/ll:wattron (charms::window-pointer charms:*standard-window*) (charms/ll:color-pair color-pair-id))
          (charms:write-string-at-point charms:*standard-window* (subseq *kiri-kawa* char-idx (1+ char-idx)) x y)
          (charms/ll:wattroff (charms::window-pointer charms:*standard-window*) (charms/ll:color-pair color-pair-id))

          (multiple-value-bind (width height)
              (charms:window-dimensions charms:*standard-window*)
            (push (list x (mod (1+ y) height) color-pair-id char-idx) *new-char-list*)
            (if (and (= y 0) (< color-pair-id +kiri-kawa-len+)) (push (list x y (1+ color-pair-id) (1+ char-idx)) *new-char-list*))
            
            
            
            ))) ; TODO
      (setf *char-list* *new-char-list* *new-char-list* nil)

      (charms:refresh-window charms:*standard-window*)
      
      (case (charms:get-char charms:*standard-window* :ignore-error t)
        ((#\q #\Q) (return)))
      (sleep 0.1))))


