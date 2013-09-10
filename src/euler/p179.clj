(ns euler.p179)

(def cap 1E7)

(def divisor-count
  (let [d (int-array (inc 1E7) 1)]
    (doseq [i (range 2 cap)
            j (range 1 (inc (quot cap i)))]
      (let [n (* i j)]
        (aset d n (inc (aget d n)))))
    d))

(dec 
  (reduce
    (fn [c n]
      (let [d1 (aget ^ints divisor-count n)
            d2 (aget ^ints divisor-count (inc n))]
        (if (== d1 d2)
          (inc c)
          c)))
    (range 2 cap)))
