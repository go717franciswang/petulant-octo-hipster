(ns euler.p164)

(def combinations 
  (memoize
    (fn [first2 first1 digits]
      (reduce +
        (for [i (range 10)
              :when (<= i first1)]
          (if (= digits 1)
            1N
            (combinations (- 9 i) (- first2 i) (dec digits))))))))

(reduce +
  (map
    (fn [i]
      (combinations (- 9 i) (- 9 i) 19))
    (range 1 10)))

