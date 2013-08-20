(ns euler.p113)

(def num-ascending 
  (memoize
    (fn [digits start-digit]
      (if (= digits 1)
        (- 10 start-digit)
        (reduce + (map #(num-ascending (dec digits) %) (range start-digit 10)))))))

(def num-descending
  (memoize
    (fn [digits start-digit]
      (if (= digits 1)
        (inc start-digit)
        (reduce + (map #(num-descending (dec digits) %) (range (inc start-digit))))))))

(defn num-non-bouncy [digits]
  (- (+ (num-ascending digits 1) (num-descending digits 9)) 10))

(reduce + (map num-non-bouncy (range 1 101)))
