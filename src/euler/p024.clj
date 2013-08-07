(ns euler.p024
  (:require [clojure.math.combinatorics :as combo]))

(read-string 
  (apply str (nth (combo/permutations [0 1 2 3 4 5 6 7 8 9]) 999999)))

