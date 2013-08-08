(ns euler.p026)

(def decimals 10000)

(def decimals-multiplier
  (reduce * (repeat decimals (bigint 10))))

(defn cycle-len [n]
  (let [rev-num (reverse (str (bigint (* (/ 1 n) decimals-multiplier))))]
    (first 
      (for [x (range 1 (int (/ decimals 3)))
            :let [parts (partition x rev-num)]
            :when (= (first parts) (second parts) (nth parts 2))]
        x))))

(first 
  (reduce 
    (fn [[max_x max_l] [x l]]
      (if (>= l max_l)
        [x l]
        [max_x max_l]))
    (for [x (range 2 1000)]
      [x (cycle-len x)])))
