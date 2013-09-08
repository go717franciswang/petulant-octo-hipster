(ns euler.p157
  (:require [euler.helper :as h]))

(def primes (h/primes (Math/sqrt 2E10)))

; p = 10^n * (a'+b') / (a'b'm)
; where a'm = a, b'm = b, coprime(a',b') => coprime(a'+b', a'b')
; iterate over coprimes that divides p and generate factors of m
(defn solutions-count [n]
  (let [coprimes (distinct 
                   (for [count2 (range (inc n))
                         :let [factor2 (reduce * (repeat count2 2))]
                         count5 (range (inc n))
                         :let [factor5 (reduce * (repeat count5 5))]
                         pair [[factor2 factor5] [1 (* factor2 factor5)]]]
                     (vec (sort pair))))]
    (reduce 
      (fn [c [a b]] 
        (let [n0 (/ (* (Math/pow 10 n) (+ a b)) a b)
              sqrt (inc (int (Math/sqrt n0)))
              factors (loop [n n0
                             primes primes
                             factors []]
                        (let [p (first primes)]
                          (if (> p sqrt)
                            (if (== n 1)
                              factors
                              (conj factors n))
                            (if (zero? (mod n p))
                              (recur (/ n p) primes (conj factors p))
                              (recur n (rest primes) factors)))))
              factor-count (frequencies factors)]
          (+ c (reduce * (map (comp inc second) factor-count)))))
      0 coprimes)))

(reduce + (map solutions-count (range 1 10)))
