(ns euler.p021)

(defn d [a]
  (reduce 
    + 
    (for [x (rest (range))
          :when (zero? (mod a x))
          :while (<= x (/ a 2))]
      x)))

(reduce
  +
  (reduce 
    (fn [s a]
      (if (contains? s a)
        s
        (let [b (d a)
              c (d b)]
          (if (and 
                (= a c)
                (not= a b))
            (conj s a b)
            s))))
    #{}
    (range 1 10000)))
