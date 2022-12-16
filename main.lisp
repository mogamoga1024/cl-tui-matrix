
(ql:quickload '(:cl-charms :cl-setlocale))

(defpackage :main
  (:use :cl :charms :cl-charms))
(in-package :main)

(defun main ()
  (cl-setlocale:set-all-to-native)

  ; (charms/ll:start-color)
  ; (charms/ll:init-pair 1 (+ 100 i) charms/ll:COLOR_BLACK)

  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-non-blocking-mode charms:*standard-window*)

    (loop :with y := 0
          :for c := (charms:get-char charms:*standard-window*
                                     :ignore-error t)
          :do (progn
                (charms:clear-window charms:*standard-window*)

                (dotimes (i (if (< y 5) y 5))
                  (charms/ll:start-color) ; TODO
                  (charms/ll:init-color (+ 100 i) 0 (* 200 (- 5 i)) 0)
                  (charms/ll:init-pair (1+ i) (+ 100 i) charms/ll:COLOR_BLACK)

                  (charms/ll:wattron (charms::window-pointer charms:*standard-window*) (charms/ll:color-pair (1+ i)))
                  (charms:write-string-at-point charms:*standard-window*
                                                "ｷﾘﾀﾝｶﾜｲｲﾔｯﾀｰ"
                                                0
                                                (- y i))
                  (charms/ll:wattroff (charms::window-pointer charms:*standard-window*) (charms/ll:color-pair (1+ i))))

                (charms:refresh-window charms:*standard-window*)

                (multiple-value-bind (width height)
                    (charms:window-dimensions charms:*standard-window*)
                  (setf y (mod (1+ y) height)))

                (case c
                  ((nil) nil)
                  ((#\q #\Q) (return)))
                (sleep 0.1)))))


