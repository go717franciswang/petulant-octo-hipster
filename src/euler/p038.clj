(ns euler.p038.clj
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

;largest pandigital number will be produced from a number that leads with 9
(defn gen-n-digit-seq [n]
  (distinct (map #(take n (cons 9 %)) (combo/permutations (range 1 9)))))

(defn pandigital [s]
  (let [n (seq-2-num s)]
    (loop [i 1
           result-seq (list)
           result-set (hash-set)]
      (if (and
            (> i 2)
            (= (count result-set) 9))
        result-seq
        (let [n2 (* i n)
              n2-seq (num-2-seq n2)
              n2-set (set n2-seq)]
          (when (and
                  (= (count n2-seq) (count n2-set))
                  (not (contains? n2-set 0))
                  (empty? (clojure.set/intersection result-set n2-set)))
            (recur (inc i) 
                   (concat result-seq n2-seq) 
                   (clojure.set/union result-set n2-set))))))))

(reduce 
  max
  (map 
    seq-2-num
    (filter identity (map pandigital (apply concat (map gen-n-digit-seq (range 1 9)))))))
