(ns euler.p039)

(def max-perimeter 1000)

(def max-a
  (inc (int (/ max-perimeter (inc (Math/pow 2 0.5))))))

(defn max-b [a]
  (inc (int (/ (- max-perimeter a) 2))))

(defn calc-c [a b]
  (Math/pow (+ (* a a) (* b b)) 0.5))

(defn int? [n]
  (= (float (int n)) n))

(reduce 
  (fn [[max-p max-count] [k v]]
    (if (> (count v) max-count)
      [k (count v)]
      [max-p max-count]))
  [0 0]
  (reduce 
    (fn [m [p sides]]
      (let [s (get m p #{})]
        (assoc m p (conj s sides))))
    {}
    (for [a (range 1 max-a)
          b (range 1 (max-b a))
          :let [c (calc-c a b)
                p (+ a b c)]
          :when (and
                  (int? c)
                  (<= p max-perimeter))]
      [(int p) (hash-set a b (int c))])))

