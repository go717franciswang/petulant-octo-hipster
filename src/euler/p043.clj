(ns euler.p043
  (:require [clojure.math.combinatorics :as combo]))

(def pandigitals
  (drop-while #(zero? (first %)) (combo/permutations (range 10))))

(defn seq-2-num [s]
  (loop [r 0
         s s]
    (if (empty? s)
      r
      (recur (+ (* 10 r) (first s)) (rest s)))))

(defn slice-3-digits-divisible-x [s n x]
  (zero? (mod (seq-2-num (take 3 (drop n s))) x)))

(defn interesting? [s]
  (and
    (slice-3-digits-divisible-x s 1 2)
    (slice-3-digits-divisible-x s 2 3)
    (slice-3-digits-divisible-x s 3 5)
    (slice-3-digits-divisible-x s 4 7)
    (slice-3-digits-divisible-x s 5 11)
    (slice-3-digits-divisible-x s 6 13)
    (slice-3-digits-divisible-x s 7 17)))

(reduce + (map seq-2-num (filter interesting? pandigitals)))
