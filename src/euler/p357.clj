(ns euler.p357
  (:require [euler.helper :as h]
            [clojure.math.combinatorics :as combo]))

; n must contain exactly one 2 b/c n/d and d must have different parity
; also no prime factor can appear more than once in n b/c their sum will be divisible by p

(def cap 1E8)

(def primes (h/primes3 (+ 2 (/ cap 2))))

(defn all-divisors [factors]
  (let [l (quot (count factors) 2)]
    (for [l0 (range 1 (inc l))
          f0 (combo/combinations factors l0)]
      (reduce * f0))))

(defn valid? [n factors]
  (every?
    (fn [d]
      (.isProbablePrime (biginteger (+ d (/ n d))) 10))
    (all-divisors factors)))

(time
(reduce
  (fn [sum p]
    (let [d (- p 2)
          n (* 2 d)]
      (cond 
        (> n cap) sum
        (not (.isProbablePrime (biginteger (inc (* 2 d))) 10)) sum
        (.isProbablePrime (biginteger d) 10) (+ sum n)
        :else (let [limit (Math/floor (Math/sqrt n))
                    factors (loop [n0 d
                                   ps (rest primes)
                                   factors '(2)]
                              (let [p (first ps)]
                                (cond
                                  (== n0 1) factors
                                  (> p limit) (conj factors n0)
                                  (zero? (mod n0 p)) (if (== p (first factors)) 
                                                       '()
                                                       (recur (/ n0 p) ps (conj factors p)))
                                  :else (recur n0 (rest ps) factors))))]
                (if (empty? factors)
                  sum
                  (if (valid? n factors)
                    (+ sum n)
                    sum))))))
  1
  (rest primes)))


