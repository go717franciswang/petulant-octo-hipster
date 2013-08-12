(ns euler.p056)

(defn pow [a b]
  (reduce * 1 (repeat b (bigint a))))

(reduce
  max
  (for [a (range 100)
        b (range 100)]
    (reduce + (map (comp read-string str) (str (pow a b))))))
