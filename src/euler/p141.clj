(ns euler.p141
  (:require [euler.helper :as h]))

(def cap 1E12)

(def positive-ints (rest (range)))

(defn square? [n]
  (let [a (Math/sqrt n)]
    (== a (int a))))

(reduce +
  (for [[b a] (rest (h/farey (int (Math/pow (dec cap) (float 1/3)))))
        c positive-ints
        :let [n (+ (* a a a b c c) (* b b c))]
        :while (< n cap)
        :when (square? n)]
    n))
