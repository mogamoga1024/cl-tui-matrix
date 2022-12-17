(ql:quickload '(:cl-charms :cl-setlocale))

(defun main ()
  (cl-setlocale:set-all-to-native)
  (charms:with-curses ()
    (let ((kiri-kawa "K1Я1+aИkawa11ya++a-")
          (kiri-kawa-len 19)
          (char-list)
          (new-char-list)
          (win charms:*standard-window*))
      (charms:disable-echoing)
      (charms:enable-raw-input)
      (charms:enable-non-blocking-mode win)

      (charms/ll:start-color)
      (dotimes (i kiri-kawa-len)
        (charms/ll:init-color (+ 8 i) 0 (* (floor 1000 kiri-kawa-len) (- kiri-kawa-len i)) 0)
        (charms/ll:init-pair (1+ i) (+ 8 i) charms/ll:COLOR_BLACK))

      (push '(0 0 1 0) char-list)

      (loop
        (multiple-value-bind (width height)
            (charms:window-dimensions win)
          (charms:clear-window win)

          (dolist (char char-list)
            (destructuring-bind (x y color-pair-id char-idx) char
              (charms/ll:wattron (charms::window-pointer win) (charms/ll:color-pair color-pair-id))
              ;; xが(1- width)かつyが(1- height)だと落ちる
              (charms:write-string-at-point win (subseq kiri-kawa char-idx (1+ char-idx)) x y)
              (charms/ll:wattroff (charms::window-pointer win) (charms/ll:color-pair color-pair-id))
              ;; 一段下げる
              (if (< (1+ y) height)
                  (push (list x (1+ y) color-pair-id (mod (1+ char-idx) kiri-kawa-len)) new-char-list))
              (if (and (= y 0) (< color-pair-id kiri-kawa-len))
                  (push (list x 0 (1+ color-pair-id) char-idx) new-char-list))))
          ;; 新しいやつ生成
          (if (= (random 2) 0)
              (push (list (random (1- width)) 0 1 (random kiri-kawa-len)) new-char-list))
          (setf char-list new-char-list new-char-list nil)

          (charms:refresh-window win)
          
          (case (charms:get-char win :ignore-error t)
            ((#\q #\Q) (return)))
          (sleep 0.1))))))


