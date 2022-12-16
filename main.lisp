
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
                (charms:clear-window charms:*standard-window*)
                (charms:write-string-at-point charms:*standard-window*
                                              "こんにちは世界"
                                              0
                                              0)
                (charms:refresh-window charms:*standard-window*)
                (case c
                  ((nil) nil)
                  ((#\q #\Q) (return)))))))


