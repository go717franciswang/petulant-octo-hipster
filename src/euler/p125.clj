(ns euler.p125)

(def cap 1E8)

(def all-nums
  (distinct
    (apply concat
      (for [x0 (rest (range))
            :let [x0sq (* x0 x0)
                  x1 (inc x0)
                  x1sq (* x1 x1)
                  sum (+ x0sq x1sq)]
            :while (< sum cap)]
        (loop [xn (inc x1)
               sums [sum]]
          (let [sum (last sums)
                next-sum (+ sum (* xn xn))]
            (if (>= next-sum cap)
              sums
              (recur (inc xn) (conj sums next-sum)))))))))

(defn palindromic? [n]
  (let [s (str n)
        l (count s)
        half (quot l 2)]
    (= (take half s) (reverse (take-last half s)))))

(reduce + (filter palindromic? all-nums))
