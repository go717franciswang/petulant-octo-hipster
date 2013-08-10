(ns euler.p049
  (:require [clojure.math.combinatorics :as combo]))

(def four-digit-primes 
  (filter 
    #(> % 999)
    (loop [p [2]
           i 3]
      (if (> i 10000)
        (set p)
        (let [sqrt (Math/pow i 0.5)]
          (if (some #(zero? (mod i %)) (take-while #(<= % sqrt) (rest p)))
            (recur p (+ i 2))
            (recur (conj p i) (+ i 2))))))))

(defn num-2-seq [n]
  (loop [r (list)
         n n]
    (if (< n 10)
      (conj r n)
      (recur (conj r (mod n 10)) (quot n 10)))))

(defn eql-spaced-seq? [s]
  (apply = (map - (butlast s) (rest s))))

(def possible-primes
  (map
    (comp sort second)
    (filter 
      (fn [[k v]]
        (> (count v) 2))
      (group-by (comp sort num-2-seq) four-digit-primes))))

(for [primes possible-primes
      four-choices (combo/combinations primes 3)
      :when (eql-spaced-seq? four-choices)]
  four-choices)

