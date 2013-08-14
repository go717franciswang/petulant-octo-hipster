(ns euler.p077
  (:require euler.helper))

; copied from p031 (the coin combination problem)

(def size 5000)

(def coin-vals
  (vec (euler.helper/primes size)))

(def ways
  (memoize
    (fn [total-val coin-vals]
      (let [use-coin-val (first coin-vals)
            max-coins-used (int (/ total-val use-coin-val))]
        (reduce
          +
          (map #(let [other-coins (rest coin-vals)
                      val-left (- total-val (* use-coin-val %))]
                  (if (empty? other-coins)
                    (if (zero? val-left) 1 0)
                    (ways val-left other-coins))) (range (inc max-coins-used))))))))

(loop [i 10]
  (let [coin-vals (take-while #(<= % i) coin-vals)]
    (let [a (ways i coin-vals)]
    (if (>= a size)
      [i a]
      (recur (inc i))))))
