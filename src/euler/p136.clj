(ns euler.p136
  (:require [euler.helper :as h]))

(def cap 5E7)

(def primes (h/primes3 cap))

(time
  (reduce 
    (fn [result p]
      (+ (if (< (* 4 p) cap) 1 0)
         (if (< (* 16 p) cap) 1 0)
         (if (zero? (mod (- p 3) 4)) 1 0)
         result))
    0
    primes))
         
