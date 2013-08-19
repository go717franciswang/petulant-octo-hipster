(ns euler.p106
  (:require [clojure.math.combinatorics :as combo]))

(def size 12)

(def a (range size))

(def A (set a))

(/ 
  (reduce +
    (map
      (fn [subset-size]
        ;(println subset-size)
        (reduce +
          (map
            (fn [b]
              (let [B (set b)
                    cs (clojure.set/difference A B)]
                (count
                  (filter
                    (fn [c]
                      ;(println c b)
                      (let [signs (map - c b)]
                        (and (some pos? signs)
                             (some neg? signs))))
                    (combo/combinations cs subset-size)))))
            (combo/combinations a subset-size))))
      (range 2 (inc (int (/ size 2)))))) 2)
