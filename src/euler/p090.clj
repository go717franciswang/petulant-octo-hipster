(ns euler.p090
  (:require [clojure.math.combinatorics :as combo]))

; since search space is so small, we can brute force it

(def dice-combos
  (for [a1 (range 5 10)
        a2 (range 4 a1)
        a3 (range 3 a2)
        a4 (range 2 a3)
        a5 (range 1 a4)
        a6 (range 0 a5)]
    #{a6 a5 a4 a3 a2 a1}))

(def square-digits [[0 1] [0 4] [0 9] [1 6] [2 5] [3 6] [4 9] [6 4] [8 1]])

(def all-digits (set (range 10)))

(defn same-digit? [a b]
  (= (if (= a 9) 6 a) 
     (if (= b 9) 6 b)))

(defn required-digits [[d1 d2] dice1]
  (let [dice1 (cond 
                (contains? dice1 6) (conj dice1 9)
                (contains? dice1 9) (conj dice1 6)
                :else dice1)
        expanded-d1 (if (same-digit? d1 6) [6 9] [d1])
        expanded-d2 (if (same-digit? d2 6) [6 9] [d2])]
    (cond 
      (and (contains? dice1 d1) (contains? dice1 d2)) [(concat expanded-d1 expanded-d2)]
      (contains? dice1 d1) [expanded-d2]
      (contains? dice1 d2) [expanded-d1]
      :else [expanded-d1 expanded-d2])))

(defn give-dice1-find-dice2 [dice1]
  (let [requirements (loop [square-digits square-digits
                            results []]
                       (if (empty? square-digits)
                         results
                         (let [[d1 d2] (first square-digits)
                               require-digit (required-digits [d1 d2] dice1)
                               require-count (count require-digit)]
                           (condp = require-count 
                             2 nil
                             0 (recur (rest square-digits) results)
                             (recur (rest square-digits) 
                                    (let [r (first require-digit)]
                                      (if (empty? results)
                                        (map hash-set r)
                                        (for [x r
                                              result results]
                                          (conj result x)))))))))
        filtered-requirements (distinct (filter #(< (count %) 7) requirements))]
    (distinct
      (for [r filtered-requirements
            :let [filler-count (- 6 (count r))
                  combos (if (zero? filler-count)
                           [nil]
                           (let [fillers (clojure.set/difference all-digits r)]
                             (combo/combinations fillers filler-count)))]
            filler combos]
        (if (nil? filler)
          r
          (apply conj r filler))))))

(count
  (distinct
    (for [dice1 dice-combos
          dice2 (give-dice1-find-dice2 dice1)]
      #{dice1 dice2})))
