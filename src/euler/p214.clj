(ns euler.p214
  (:require [euler.helper :as h]))

(def cap 4E7)
(def length 25)

(def primes (h/primes3 cap))

(def totients
  (let [t (int-array (inc cap) (range (inc cap)))]
    (doseq [p primes
            :let [r (/ (dec p) p)]
            n (range p (inc cap) p)]
      (aset t n (int (* (aget t n) r))))
    t))

(def chain-length 
  (memoize
    (fn [n]
      (if (== n 1)
        1
        (inc (chain-length (aget ^ints totients n)))))))

(reduce
  (fn [s p]
    (let [v (chain-length p)]
      (if (== v length) (+ s p) s)))
  0
  primes)

