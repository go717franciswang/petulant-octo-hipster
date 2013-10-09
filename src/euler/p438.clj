(ns euler.p438
  (:require [clojure.math.combinatorics :as c]))

(def n 5)

(def base (reduce + (range 1 (inc n))))

(defn sum-nums [nums i]
  (let [combos (c/combinations nums i)]
    (reduce + (map #(reduce * %) combos))))

(defn min-ai [delta i]
  (let [nums (concat (range 1 (inc (- n delta)))
                     (range (+ 2 (- n delta)) (+ 2 n)))]
    (sum-nums nums i)))

(defn max-ai [delta i]
  (let [nums (map #(+ % (/ delta n)) (range 1 (inc n)))]
    (sum-nums nums i)))

(count
(apply concat 
(for [delta (range n)]
  (if (zero? delta)
    [(map (partial min-ai 0) (range 1 (inc n)))]
    (let [ranges
      (loop [i 2
             ranges [(range (min-ai delta 1) 
                            (inc (max-ai delta 1)))]]
        (if (> i n)
          ranges
          (recur 
            (inc i) 
            (conj ranges 
                  (range (inc (min-ai delta i))
                         (inc (Math/floor 
                                (max-ai delta i))))))))]
      (apply c/cartesian-product ranges))))))
    ; (count
    ; (apply c/cartesian-product ranges))))

;(min-ai 0 1)
