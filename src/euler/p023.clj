(ns euler.p023)

(defn divisibles-sum [n]
  (reduce + (filter #(zero? (mod n %)) (range 1 (inc (int (/ n 2)))))))

(defn abundant? [n]
  (> (divisibles-sum n) n))

(def abundants
  (apply sorted-set (filter abundant? (range 1 28123))))

(defn sum-of-2-abundants? [n]
  (some #(contains? abundants (- n %)) (take-while #(<= % (/ n 2)) abundants)))

(reduce + (filter (comp not sum-of-2-abundants?) (range 1 28123)))
  

