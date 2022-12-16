
(ql:quickload '(:cl-charms :cl-setlocale))

(defun main ()
  (cl-setlocale:set-all-to-native)
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-non-blocking-mode charms:*standard-window*)

    (loop :for c := (charms:get-char charms:*standard-window*
                                     :ignore-error t)
          :do (progn
                ;; Redraw time
                (charms:clear-window charms:*standard-window*)
                ;(paint-time)
                (charms:refresh-window charms:*standard-window*)

                ;; Process input
                (case c
                  ((nil) nil)
                  ;((#\Space) (start/stop/clear))
                  ((#\q #\Q) (return)))))))


