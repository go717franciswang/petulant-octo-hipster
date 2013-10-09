(ns euler.p345
  (:require [clojure.string :as s]))

(def data "7  53 183 439 863
          497 383 563  79 973
          287  63 343 169 583
          627 343 773 959 943
          767 473 103 699 303")

(def matrix (vec (map 
                   #(vec (map read-string (s/split (s/trim %) #"\s+"))) 
                   (s/split data #"\n"))))

(defn drop-col [matrix y]
  (map 
    (fn [row]
      (

(defn best [matrix size]
  (if (= size 2)
    (max (+ (get-in matrix [0 0])
            (get-in matrix [1 1]))
         (+ (get-in matrix [1 0])
            (get-in matrix [0 1])))
    (for [x (range size)
          y (range size)
          :let [x (+ x0 dx)
                y (+ y0 dy)]]
      (

