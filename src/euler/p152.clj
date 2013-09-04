(ns euler.p152
  (:require [euler.helper :as h]
            [clojure.math.combinatorics :as combo]))

(def cap 80)

(def primes (h/primes cap))

(def useless-primes (cons 11 (drop-while #(< % 14) primes)))
(defn useful-num? [n]
  (not (some #(zero? (mod n %)) useless-primes)))

(defn inverse-sq-sum [nums]
  (reduce + (map #(/ 1 % %) nums)))

(defn num-set [p]
  (let [p-multiples (filter useful-num? (range (* 2 p) (inc cap) p))
        result (map #(set %) (filter
                 #(not= 0 (mod (denominator (inverse-sq-sum %)) p))
                 (reduce
                   (fn [result n]
                     (concat result (map #(conj % n) result) [[n]]))
                   [[p]]
                   p-multiples)))]
    (vec (distinct result))))

(defn not-divisible-by [nums q]
  (every? #(not= 0 (mod % q)) nums))

(defn join-set [as bs q]
  (for [a as
        b bs
        :when (not-divisible-by (clojure.set/difference a b) q)]
    (clojure.set/union a b)))

(def init-result
  (loop [n 2
         result []]
    (if (> n cap)
      (conj (set (map (fn [nums]
                  (reduce + (map #(/ 1 % %) nums)))
                result)) 0)
      (recur (* n 2) (concat result (map #(conj % n) result) [#{n}])))))

(def candidates
  (reduce
    (fn [results p]
      (println p (count results))
      (let [nums (num-set p)]
        (concat results nums (if (some empty? [results nums]) [] (join-set results nums p)))))
    []
    [3 5 7 13]))

(defn valid? [candidate]
  (let [a (reduce + (map #(/ 1 % %) candidate))]
    (get init-result (- 1/2 a))))

(count (set (filter valid? candidates)))

