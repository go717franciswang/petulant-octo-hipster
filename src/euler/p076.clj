(ns euler.p076)

; copied from p031 (the coin combination problem)

(def coin-vals
  (vec (rest (range 100))))

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

(doseq [n (rest (range 100))]
  (ways n coin-vals))

(ways 100 coin-vals)
