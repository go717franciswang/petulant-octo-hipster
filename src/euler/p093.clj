(ns euler.p093
  (:require [clojure.math.combinatorics :as combo]))

(def max-num 100)

(def operations [+ - * /])

(def operation-seq
  (for [f1 operations
        f2 operations
        f3 operations]
    [f1 f2 f3]))

(def num-sets
  (for [n1 (range 4 (inc max-num))
        n2 (range 3 n1)
        n3 (range 2 n2)
        n4 (range 1 n3)]
    [n4 n3 n2 n1]))

(defn pos-int? [n] (and (> n 0) (= (float (int n)) n)))

(defn partition-seq [s]
  (loop [se (rest s)
         r [[(first s)]]]
    (if (empty? se)
      r
      (let [a (first se)
            b (last r)
            c (count r)]
        (if (= a (inc (last b)))
          (recur (rest se) (assoc r (dec c) (conj b a)))
          (recur (rest se) (conj r [a])))))))

(defn all-answers [num-set]
  (let [answers (distinct
                  (for [fs operation-seq
                        nums (combo/permutations num-set)]
                    (loop [num-set (rest nums)
                           fs fs
                           result (float (first nums))]
                      (if (empty? num-set)
                        result
                        (recur (rest num-set) (rest fs) ((first fs) result (first num-set)))))))
        pos-int-answers (sort (map int (filter pos-int? answers)))]
    pos-int-answers))

(defn longest-answer-set [num-set]
  (reduce max (map count (partition-seq (all-answers num-set)))))

(first
  (reverse
    (sort-by second
      (for [num-set num-sets]
        (let [a (longest-answer-set num-set)]
          (println num-set a)
          [num-set a])))))
