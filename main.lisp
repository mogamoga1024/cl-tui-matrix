
(ql:quickload '(:cl-charms :cl-setlocale))

(defun main ()
  (cl-setlocale:set-all-to-native)
  (format t "hello~%"))


