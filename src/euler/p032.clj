(ns euler.p032
  (:require [clojure.math.combinatorics :as combo]))

(defn seq-2-num [s]
  (loop [r 0
         s s]
    (if (empty? s)
      r
      (recur (+ (* 10 r) (first s)) (rest s)))))

(defn num-2-seq [n]
  (loop [r (list)
         n n]
    (if (< n 10)
      (conj r n)
      (recur (conj r (mod n 10)) (quot n 10)))))

(def pandigital-set
  (set (range 1 10)))

(reduce
  +
  (reduce
    (fn [pan-set [s1 s2]]
      (let [n1 (seq-2-num s1)
            n2 (seq-2-num s2)
            n3 (* n1 n2)]
        (let [s (concat (num-2-seq n3) s1 s2)]
          (if (and
                (= (count s) 9)
                (= (set s) pandigital-set))
            (conj pan-set n3)
            pan-set))))
    #{}
    (concat
      (distinct (map #(split-at 1 (take 5 %)) (combo/permutations (range 1 10))))
      (distinct (map #(split-at 2 (take 5 %)) (combo/permutations (range 1 10)))))))
