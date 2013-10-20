(ns euler.p441
  (:require [euler.helper :as h]))

(def N 1000)

; for every p from 1 to 10^7 - 1
;   for every q from p+1 up to 10^7
;     if they are coprime
;       compute 1/(pq)
;       there are exactly min(p+1, N-q+1) M's that satisfy this condition
;       yield (1+p)/(pq)
(println
(time
(reduce +
  (for [p (range 1 N)
        q (range (inc p) (inc N))
        :when (= (h/gcd p q) 1)
        :let [c (inc (min p (- N q)))]]
    (double (/ c p q))))))

(time
(reduce + -2
(for [[p q] (rest (h/farey N))
      :let [c (inc (min p (- N q)))]]
  (double (/ c p q)))))
