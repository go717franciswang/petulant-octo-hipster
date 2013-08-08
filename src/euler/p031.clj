(ns euler.p031)

(def coin-vals
  [1 2 5 10 20 50 100 200])

(defn ways [total-val coin-vals]
  (let [use-coin-val (first coin-vals)
        max-coins-used (int (/ total-val use-coin-val))]
    (reduce
      +
      (map #(let [other-coins (rest coin-vals)
                  val-left (- total-val (* use-coin-val %))]
              (if (empty? other-coins)
                (if (zero? val-left) 1 0)
                (ways val-left other-coins))) (range (inc max-coins-used))))))

(ways 200 coin-vals)
