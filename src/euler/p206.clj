(ns euler.p206
  (:require [clojure.math.combinatorics :as combo]))

(def pattern "1.2.3.4.5.6.7.8.9.0$")

(loop [l 1
       candidates [0]]
  (println l (count candidates))
  (if (== l 10)
    (let [patt (re-pattern pattern)]
      (filter #(re-matches patt (str (* % %))) candidates))
    (let [patt (re-pattern (str ".*" (subs pattern (- (count pattern) (+ 2 l)))))
          m (reduce * (repeat l 10N))]
      (recur 
        (inc l)
        (for [candidate candidates
              digit (range 10)
              :let [n (+ (* m digit) candidate)
                    s (str (* n n))]
              :when (re-matches patt s)]
          n)))))
