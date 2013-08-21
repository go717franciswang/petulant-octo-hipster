(ns euler.p117)

(def min-length 2)

(def colored-start 
  (memoize
    (fn [length]
      (if (< length min-length)
        0
        (reduce +
          (for [colored-length [2 3 4]
                black-length (range (inc (- length colored-length)))
                :let [left (- length colored-length black-length)]]
            (if (zero? left)
              1
              (colored-start left))))))))

(defn any-start [length]
  (inc 
    (reduce +
      (for [black-length (range (inc (- length min-length)))
            :let [left (- length black-length)]]
        (colored-start left)))))

(any-start 50)
