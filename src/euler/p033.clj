(ns euler.p033)

(denominator
  (reduce
    *
    (for [x (range 10 100)
          y (range 10 100)
          :let [x1 (quot x 10)
                x2 (rem x 10)
                y1 (quot y 10)
                y2 (rem y 10)]
          :when (and
                  (= x2 y1)
                  (not= y2 0)
                  (= (/ x y) (/ x1 y2)))]
      (/ x y))))
