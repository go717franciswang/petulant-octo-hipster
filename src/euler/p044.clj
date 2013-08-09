(ns euler.p044
  (:require [clojure.math.combinatorics :as combo]))

(def pentagonals
  (apply sorted-set (map #(int (* 0.5 % (dec (* 3 %)))) (range 1 10000))))

(reduce
  min
  (map 
    second
    (filter 
      (fn [[sum diff]]
        (and
          (contains? pentagonals sum)
          (contains? pentagonals diff))) 
      (map 
        (fn [[a b]]
          [(+ a b) (- b a)])
      (combo/combinations (take 3000 pentagonals) 2)))))
