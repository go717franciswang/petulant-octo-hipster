(ns euler.p087
  (:require euler.helper))

; n = a^2 + b^3 + c^4

(def max-sum 5E7)

(def primes (euler.helper/primes (Math/pow (- max-sum (Math/pow 2 4) (Math/pow 2 3)) 0.5)))

(count
  (distinct 
    (for [c primes
          :let [c4 (Math/pow c 4)]
          :while (< c4 max-sum)
          b primes
          :let [b3 (Math/pow b 3)]
          :while (< (+ c4 b3) max-sum)
          a primes
          :let [a2 (Math/pow a 2)
                t (+ c4 b3 a2)]
          :while (< t max-sum)]
      t)))
