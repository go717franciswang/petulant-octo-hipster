(ns euler.p162
  (:require [euler.helper :as h]))

(def only-leading-zero
  (reduce +
    (for [i (range 3 17)
          a (range 1 (- i 1))
          b (range 1 (- i a))
          :let [c (- i a b)]] ; let c be count leading 0s
      (reduce * 
              (/ 
                (* (h/C (- 16 c) (+ a b)) (h/P (+ a b) (+ a b))) 
                (h/P a a) 
                (h/P b b)) 
              (repeat (- 16 i) 13)))))


(.toUpperCase
  (format "%x"
    (biginteger
      (reduce + (- only-leading-zero)
        (for [i (range 3 17)
              a (range 1 (- i 1))
              b (range 1 (- i a))
              :let [c (- i a b)]] 
            (reduce * (/ (* (h/C 16 i) (h/P i i)) (h/P a a) (h/P b b) (h/P c c)) (repeat (- 16 i) 13)))))))

