(ns euler.p153
  (:require [euler.helper :as h]))

; product of conjugates always equal to an integer
; let primitive conjugates be a,b s.t a and b are coprime 
; all other conjugates can be obtained by scaling by k
; no product of two primitives can be an integer
; leaving us to look at only primitive conjugates
; since cap is 1E8, a^2 <= cap, it makes the search space quite small

(def cap 1E5)

(for [a (range 1 (inc (int (Math/sqrt cap))))
      :let [aa (* a a)
            max-bb (- n aa)]
      b (range (inc (int (Math/sqrt max-bb))))
      :when (== 1 (gcd a b))
      :let [max-k (int (/ cap (- max-bb (* b b))))]
