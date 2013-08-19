(ns euler.p105
  (:require [clojure.math.combinatorics :as combo]))

(def sets
  (map
    (fn [line]
      (map read-string (clojure.string/split line #",")))
    (clojure.string/split
      (slurp (.getFile (clojure.java.io/resource "sets.txt")))
      #"\n")))

(defn sum [s] (reduce + s))

(defn valid-equal-sized-set? [a]
  (let [size (count a)
        max-subset-size (int (/ size 2))
        A (set a)]
    (every?
      (fn [subset-size]
        (every?
          (fn [b]
            (let [B (set b)
                  sum-b (sum b)
                  cs (clojure.set/difference A B)]
              (every? 
                (fn [c]
                  (not= sum-b (sum c)))
                (combo/combinations cs subset-size))))
          (combo/combinations a subset-size)))
        (range 2 (inc max-subset-size)))))

(defn valid-greater-sized-set? [a]
  (let [size (count a)
        max-subset-size (int (/ size 2))
        A (set a)]
    (every?
      (fn [subset-size]
        (every?
          (fn [b]
            (let [B (set b)
                  sum-b (sum b)
                  cs (clojure.set/difference A B)]
              (every? #(< sum-b (sum %)) (combo/combinations cs (inc subset-size)))))
          (combo/combinations a subset-size)))
      (range 1 (inc max-subset-size)))))

(defn valid? [a]
  (and (valid-equal-sized-set? a) 
       (valid-greater-sized-set? a)))

(time
  (reduce + (map sum (filter valid? sets))))
