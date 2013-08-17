(ns euler.p092
  (:require euler.helper))

(def chain-89?
  (memoize
    (fn [n]
      (cond
        (= n 89) true
        (= n 1) false
        :else (recur (reduce + (map #(* % %) (euler.helper/num-2-seq n))))))))

(count
  (filter #(do (when (zero? (mod % 1E4)) (println %)) (chain-89? %)) (range 1 1E7)))
