(ns euler.p103
  (:require [clojure.math.combinatorics :as combo]))

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

(def predicted-answer [20 31 38 39 40 42 45])
(def upper-bound (sum predicted-answer))

; we know that [20 31 38 39 40 42 45] is a valid set but may not be optimal
; try to find a solution that is close to this set

(defn around [n]
  (reverse
    (for [i (range -3 4)]
      (+ n i))))

(def candidates
  (filter #(<= (sum %) upper-bound)
    (loop [answers (rest predicted-answer)
           rs (map vector (around (first predicted-answer)))]
      (if (empty? answers)
        rs
        (recur 
          (rest answers)
          (for [r rs
                :let [floor (last r)]
                a (around (first answers))
                :while (> a floor)]
            (conj r a)))))))

(first 
  (sort-by sum
    (filter valid? candidates)))
