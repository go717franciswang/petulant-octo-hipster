(ns euler.p075
  (:require euler.helper))

; Euclid's formula 
; a = k(mm - nn), b = k2mn, c = k(mm + nn) => L = 2kmm + 2kmn => L = 2km(m + n)

(def size 1500000)

(def m-cap (int (Math/pow (/ size 2) 0.5)))

(def primes (euler.helper/primes m-cap))

(count
  (filter (comp (partial = 1) second)
    (frequencies
      (for [[n m] (euler.helper/farey (* 2 m-cap))
            :let [l (* 2 m (+ m n))]
            :when (and (odd? (- m n)) (> n 0) (> m n))
            k (range 1 (inc (int (/ size l))))]
          (* l k)))))
