(ns euler.p204
  (:require [euler.helper :as h]))

(def cap 1E9)
(def hamming 100)

(def primes (h/primes3 hamming))

(loop [nums [1]
       primes primes]
  (if (empty? primes)
    (count nums)
    (let [p (first primes)]
      (recur (into nums (for [n nums
                              n2 (iterate #(* p %) p)
                              :let [n3 (* n n2)]
                              :while (<= n3 cap)]
                          n3))
             (rest primes)))))
