(ns euler.p381
  (:require [euler.helper :as h]))

(defn factorial [n p]
  (reduce 
    (fn [a b]
      (mod (* a b) p))
    1N
    (range 1 (inc n))))

(defn S [p]
  (mod (reduce + (map #(factorial % p) (range (- p 5) p))) p))

(def primes (drop 2 (h/primes3 100)))

(println (map S primes))

(reduce + (map S primes))
