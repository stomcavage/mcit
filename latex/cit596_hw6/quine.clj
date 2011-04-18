;     Program: quine.clj
; Description: A program that outputs a copy of itself. The program consists of
;              an anonymous function that takes the string "w" and prints 
;              "((w (quote w)))". The argument "w" to the anonymous function
;              is the full text of the anonymous function itself as a quoted
;              string. This all works because code is data is code in Clojure.

((fn [w] (list w (list (quote quote) w))) 
  (quote (fn [w] (list w (list (quote quote) w)))))
