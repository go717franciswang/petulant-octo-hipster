(ns euler.p115)

(def min-block-size 50)

(def ways-red-start 
  (memoize
    (fn [length]
      (if (< length min-block-size)
        0
        (reduce +
          (for [red-length (range min-block-size (inc length))
                black-length (range (inc (- length red-length)))
                :let [left (- length red-length black-length)]]
            (if (zero? left) 
              1 
              (if (> black-length 0) 
                (ways-red-start left)
                0))))))))

(defn ways-any-start [length]
  (inc
    (reduce +
      (map
        (fn [black-length]
          (let [left (- length black-length)]
            (ways-red-start left)))
        (range (inc length))))))

(loop [n 50]
  (if (> (ways-any-start n) 1E6)
    n
    (recur (inc n))))
