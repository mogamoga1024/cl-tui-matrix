
(ql:quickload '(:cl-charms :cl-setlocale))

(defpackage :main
  (:use :cl :charms :cl-charms))
(in-package :main)

(defparameter *y* 0)

(defun main ()
  (cl-setlocale:set-all-to-native)

  ; (charms/ll:start-color)
  ; (charms/ll:init-pair 1 charms/ll:COLOR_GREEN charms/ll:COLOR_BLACK)

  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-non-blocking-mode charms:*standard-window*)

    (loop :for c := (charms:get-char charms:*standard-window*
                                     :ignore-error t)
          :do (progn
                (charms:clear-window charms:*standard-window*)

                (charms/ll:start-color) ; TODO
                (charms/ll:init-color charms/ll:COLOR_GREEN 0 1000 0)
                (charms/ll:init-pair 1 charms/ll:COLOR_GREEN charms/ll:COLOR_BLACK)


                (charms/ll:wattron (charms::window-pointer charms:*standard-window*) (charms/ll:color-pair 1))
                (charms:write-string-at-point charms:*standard-window*
                                              "こんにちは世界"
                                              0
                                              *y*)
                (charms/ll:wattroff (charms::window-pointer charms:*standard-window*) (charms/ll:color-pair 1))
                
                (charms:refresh-window charms:*standard-window*)

                (incf *y*)

                (case c
                  ((nil) nil)
                  ((#\q #\Q) (return)))
                (sleep 0.1)))))


