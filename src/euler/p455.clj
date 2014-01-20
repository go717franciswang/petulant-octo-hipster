(ns euler.p455
  (:require [euler.helper :as h]))

(loop [i 1
       a #{}]
  (let [e (h/pow-mod 4 i (bigint 1E9))]
    (println i e)
    (if (contains? a e)
      (println "duplicate found")
      (recur (inc i) (conj a e)))))


