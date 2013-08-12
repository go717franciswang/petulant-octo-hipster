(ns euler.p063)

(defn pow [a n]
  (reduce * 1 (repeat n (bigint a))))

(defn n-digit-nth-power [n]
  (count (take-while #(= % n) (drop-while #(< % n) (map #(count (str (pow % n))) (range))))))

; subtract one b/c 0 should not be counted
(dec (reduce + (take-while #(not= 0 %) (map n-digit-nth-power (rest (range))))))

