(ns euler.p127
  (:require [euler.helper :as h]))

(def cap 120000)

(def primes (h/primes cap))

(defn prime-factors [n]
  (loop [n n
         ps primes
         result #{}]
    (if (= n 1)
      result
      (let [p (first ps)]
      (if (zero? (mod n p))
        (recur (/ n p) ps (conj result p))
        (recur n (rest ps) result))))))

(def rad 
  (memoize
    (fn [n]
      (reduce * (prime-factors n)))))

(defn sum-abc-hits [c]
  (let [rc (rad c)]
    (if (== rc c)
      0
      (reduce +
        (for [a (rest (range))
              :let [b (- c a)]
              :while (> b a)
              :when (and (== (h/gcd a b) (h/gcd a c) (h/gcd b c) 1)
                         (< (* (rad a) (rad b) (rad c)) c))]
          c)))))

(time
  (reduce +
    (pmap sum-abc-hits (range 3 (inc cap)))))
