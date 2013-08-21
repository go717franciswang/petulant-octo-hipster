(ns euler.p121
  (:require [clojure.math.combinatorics :as combo]))

(def turns 15)

(def possibilities (reduce * (repeat turns 2)))

(def wins-required (int (Math/floor (inc (/ turns 2)))))

(defn prob-of [permutation]
  (reduce *
    (map-indexed
      (fn [idx itm]
        (let [inv (/ 1 (+ idx 2))]
          (if (zero? itm)
            (- 1 inv)
            inv)))
      permutation)))

(defn bit-seq [bits]
  (map (fn [pos] (if (bit-test bits pos) 1 0)) (range turns)))

(def prob 
  (reduce +
    (for [bits (range possibilities)
          :let [permutation (bit-seq bits)]
          :when (>= (reduce + permutation) wins-required)]
      (prob-of permutation))))

(int (Math/floor (float (/ 1 prob))))
