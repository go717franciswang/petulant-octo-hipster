(ns euler.p078
  (:require euler.helper))

; copied from p031 (the coin combination problem)

(def divisible-by 1000000)

(def ways-cache (atom {}))

(def ways
  (fn 
    ([total-val max-coin-val]
      (let [max-coins-used (int (/ total-val max-coin-val))]
        ;(println use-coin-val max-coins-used)
        (reduce
          +
          (map 
            #(let [next-coin-val (dec max-coin-val)
                   val-left (- total-val (* max-coin-val %))]
               (cond
                 (> next-coin-val val-left) 0
                 (<= val-left 1) val-left
                 (> val-left next-coin-val) (ways val-left next-coin-val)
                 :else (get @ways-cache val-left)))
            (reverse (range (inc max-coin-val)))))))))

(loop [i 1]
  (let [coin-vals (bigint i)
        a (ways (bigint i) coin-vals)]
    (swap! ways-cache assoc i a)
    (println [i a])
    (if (or (= i 10) (zero? (mod a divisible-by)))
      [i a]
      (recur (inc i)))))
