(ql:quickload '(:cl-charms :cl-setlocale))
(defpackage :main (:use :cl :charms :charms/ll :cl-setlocale)
(:shadow :curses-version))(in-package :main)

(defun main()(set-all-to-native)(with-curses()(let*((e *standard-window*)(c)(d)
(a "K1Я1+aИkawa11ya++a-")(b(length a))(f(charms::window-pointer e))(g))
(disable-echoing)(enable-raw-input)(enable-non-blocking-mode e)(start-color)
(dotimes(i b)(init-color(+ 8 i)0(*(floor 1000 b)(- b i))0)(init-pair(1+ i)(+ 8 
i)0))(loop(multiple-value-bind(h j)(window-dimensions e)(clear-window e)(dolist
(m c)(destructuring-bind(x y k l)m(let((o(color-pair k)))(wattron f o)
(write-string-at-point e(subseq a l(1+ l))x y)(wattroff f o))(if(<(1+ y)j)(push
(list x(1+ y)k(mod(1+ l)b))d))(if(and(= y 0)(< k b))(push(list x 0(1+ k)l)d))))
(setf g(random(1- h)))(if(every #'(lambda(n)(or(/=(car n)g)(/=(cadr n)0)))d)
(push(list g 0 1(random b))d))(setf c d d nil)(refresh-window e)(case(get-char
e :ignore-error t)((#\q #\Q)(return)))(sleep 0.1))))))
