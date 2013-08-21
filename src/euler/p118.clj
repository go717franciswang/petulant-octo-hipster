(ns euler.p118
  (:require euler.helper
            [clojure.math.combinatorics :as combo]))

(defn prime-partitions [num-seq min-partition-length]
  (if (zero? (count num-seq))
    [[]]
    (for [first-partition-length (range min-partition-length (inc (count num-seq)))
          :let [first-partition (take first-partition-length num-seq)
                first-num (read-string (apply str first-partition))]
          :when (.isProbablePrime (biginteger first-num) 15)
          other-partitions (prime-partitions (drop first-partition-length num-seq) first-partition-length)]
      (cons first-num other-partitions))))

(loop [num-seq (combo/permutations (range 1 10))
       results #{}]
  (if (empty? num-seq)
    (count results)
    (recur
      (rest num-seq)
      (let [valid-prime-seq (prime-partitions (first num-seq) 1)]
        (reduce 
          (fn [results partitions]
            (conj results (set partitions)))
          results
          valid-prime-seq)))))
