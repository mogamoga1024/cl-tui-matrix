
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

    ; x y pair-id char-idx
    (push '(0 0 1 0) *char-list*)

    (loop :with y := 0
          :do (progn
                (charms:clear-window charms:*standard-window*)

                (dotimes (i (if (< y +kiri-kawa-len+) y +kiri-kawa-len+))
                  (charms/ll:wattron (charms::window-pointer charms:*standard-window*) (charms/ll:color-pair (1+ i)))
                  (charms:write-string-at-point charms:*standard-window* *kiri-kawa* 0 (- y i))
                  (charms/ll:wattroff (charms::window-pointer charms:*standard-window*) (charms/ll:color-pair (1+ i))))

                (charms:refresh-window charms:*standard-window*)

                (multiple-value-bind (width height)
                    (charms:window-dimensions charms:*standard-window*)
                  (setf y (mod (1+ y) height)))

                (case (charms:get-char charms:*standard-window* :ignore-error t)
                  ((#\q #\Q) (return)))
                (sleep 0.1)))))


