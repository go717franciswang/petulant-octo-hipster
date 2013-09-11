(ns euler.p203
  (:require [euler.helper :as h]))

(def rows 51)

(def pascal-nums
  (loop [nums #{}
         row [1N]]
    (if (== rows (count row))
      nums
      (let [row (map + (concat [0] row) (concat row [0]))]
        (recur (into nums row) row)))))

;(def primes (h/primes3 (inc (int (Math/sqrt (/ (reduce max pascal-nums) 2))))))
(def primes (h/primes3 100)) ; somehow only needed to test very small primes

(def primes-sq (map #(* % %) primes))

(defn squarefree? [n]
  (not (some (fn [d] (zero? (mod n d))) (take-while #(<= % n) primes-sq))))

(reduce + (filter squarefree? pascal-nums))
