(ns euler.p048)

(def preserve-last-n-digits 10)

(def overflow-at
  (reduce * (repeat preserve-last-n-digits (bigint 10))))

(defn truncate-num [n]
  (if (< n overflow-at)
    n
    (let [truncated (- n (* (quot n overflow-at) overflow-at))]
      (if (zero? truncated)
        overflow-at
        truncated))))

(defn truncated-multiply [a b]
  (truncate-num (* a b)))

(truncate-num (reduce + (map #(reduce truncated-multiply (repeat % %)) (range 1 1001))))
