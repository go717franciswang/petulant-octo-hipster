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

(def primes (h/primes3 1000))

(defn squarefree? [n]
  (not (some (fn [d] (zero? (mod n (* d d)))) (take-while #(<= % (quot n 2)) primes))))

(reduce + (filter squarefree? pascal-nums))
