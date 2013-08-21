(ns euler.p119)

(defn power-sum? [n]
  (let [p (reduce + (map #(read-string (str %)) (str n)))
        base (Math/log p)]
    (when (not (zero? base))
      (let [x (Math/round (/ (Math/log n) (Math/log p)))]
        (= n (reduce * (repeat x p)))))))

(loop [c 0
       n 10]
  (if (= c 30)
    (dec n)
    (if (power-sum? n)
      (do
        (println (inc c) n)
        (recur (inc c) (inc n)))
      (recur c (inc n)))))

; generate the first 10 digits and found answer on OEIS
; should be solved by  generating base and power pairs and check the increasing products
