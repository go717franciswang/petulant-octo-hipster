(ns euler.p436)

; brute force

(defn simulate []
  (loop [s 0.0
         x nil]
    (let [x0 (rand)
          s0 (+ s x0)]
      (cond
        (> s0 2) (> x x0)
        (> s0 1) (if x
                   (recur s0 x)
                   (recur s0 x0))
        :else (recur s0 x)))))

(defn simulate-many [times]
  (loop [n 0
         c 0N]
    (if (< n (inc times))
      (if (simulate)
        (recur (inc n) (inc c))
        (recur (inc n) c))
      c)))

(def batch-size 100000)

(reduce
  (fn [[tw t] w]
    (let [tw (+ tw w)
          t (+ t batch-size)]
      (println tw t (double (/ tw t)))
      [tw t]))
  [0 0]
  (pmap simulate-many (repeat batch-size)))
