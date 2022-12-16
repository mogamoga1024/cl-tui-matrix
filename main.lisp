
(ql:quickload '(:cl-charms :cl-setlocale))

(defpackage :main
  (:use :cl :charms :cl-charms))
(in-package :main)

(defparameter *kiri-kawa* "ｷﾘﾀﾝｶﾜｲｲﾔｯﾀｰ")
(defconstant +kiri-kawa-len+ (length *kiri-kawa*))
(defparameter *char-list* nil)

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

    (loop (setf y (second (car *char-list*))) ; TODO
          (setf color-pair-id (third (car *char-list*))) ; TODO
          
          (charms:clear-window charms:*standard-window*)

          (charms/ll:wattron (charms::window-pointer charms:*standard-window*) (charms/ll:color-pair color-pair-id))
          (charms:write-string-at-point charms:*standard-window* *kiri-kawa* 0 y)
          (charms/ll:wattroff (charms::window-pointer charms:*standard-window*) (charms/ll:color-pair color-pair-id))

          (charms:refresh-window charms:*standard-window*)

          (multiple-value-bind (width height)
              (charms:window-dimensions charms:*standard-window*)
            (setf (second (car *char-list*)) (mod (1+ y) height))) ; TODO

          (case (charms:get-char charms:*standard-window* :ignore-error t)
            ((#\q #\Q) (return)))
          (sleep 0.1))))


