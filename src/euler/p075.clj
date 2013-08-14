(ns euler.p075
  (:require euler.helper))

; Euclid's formula 
; a = k(mm - nn), b = k2mn, c = k(mm + nn) => L = 2kmm + 2kmn => L = 2km(m + n)
; L with only one possible triple => L/2 is a product of 2 uniq numbers (either a prime or 1)
; while m > n

(def size 1500000)
;(def size 150)

(def m-cap (int (Math/pow (/ size 2) 0.5)))

(def primes (euler.helper/primes m-cap))

(count
  (filter (comp (partial = 1) count second)
    (reduce
      (fn [m [l s]]
        (let [v (conj (get m l #{}) s)]
          (assoc m l v)))
      {}
      (for [[n m] (euler.helper/farey m-cap)
            :let [l (* 2 m (+ m n))]
            :when (and (odd? (- m n)) (> n 0))
            k (range 1 (inc (int (/ size l))))]
        (let [a (* k (- (* m m) (* n n)))
              b (* k m n)
              c (* k (+ (* m m) (* n n)))]
          [l #{a b c}])))))
