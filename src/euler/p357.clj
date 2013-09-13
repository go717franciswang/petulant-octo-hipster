(ns euler.p357
  (:require [euler.helper :as h]
            [clojure.math.combinatorics :as combo]))

; n must contain exactly one 2 b/c n/d and d must have different parity
; also no prime factor can appear more than once in n b/c their sum will be divisible by p

(def cap 1E8)

(def primes (h/primes3 (+ 2 (/ cap 2))))

(defn all-divisors [factors]
  (let [l (quot (count factors) 2)]
    (for [l0 (range 1 (+ 2 l))
          f0 (combo/combinations factors l0)]
      (reduce * f0))))

(defn valid? [n factors]
  (every?
    (fn [d]
      (.isProbablePrime (biginteger (+ d (/ n d))) 15))
    (all-divisors factors)))

(reduce
  (fn [sum p]
    (let [d (- p 2)
          n (* 2 d)]
      (cond 
        (> n cap) sum
        (not (.isProbablePrime (biginteger (inc (* 2 d))) 15)) sum
        (.isProbablePrime (biginteger d) 15) (do (println n) (+ sum n))
        :else (let [limit (Math/floor (Math/sqrt n))
                    factors (loop [n0 n
                                   ps (rest primes)
                                   factors '(2)]
                              (let [p (first ps)]
                                (if (or (> p limit) (== p 1))
                                  factors
                                  (let [mod0 (zero? (mod n0 p))]
                                    (cond 
                                      (and mod0 (== p (first factors))) []
                                      mod0 (recur (/ n0 p) ps (conj factors p))
                                      :else (recur n0 (rest ps) factors))))))]
                (if (empty? factors)
                  sum
                  (if (valid? n factors)
                    (do (println n) (+ sum n))
                    sum))))))
  1
  (rest primes))


