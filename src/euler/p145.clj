(ns euler.p145)

(def pairs-count 
  (memoize
    (fn [carry-over? with-carry-over? start]
      (let [predicate (fn [n]
                        (let [n (if with-carry-over? (inc n) n)]
                          (and (odd? n)
                               ((if carry-over? > <) n 10))))]
      (count (for [a (range start 10)
                   b (range start 10)
                   :let [s (+ a b)]
                   :when (predicate s)]
               s))))))

(defn rev-count [digits]
  (cond
    (= digits 1) 0
    (even? digits) (* (pairs-count false false 1)
                      (int (Math/pow (pairs-count false false 0) (/ (- digits 2) 2))))
    (= (mod digits 4) 1) 0
    :else (* 5 
            (pairs-count true false 1)
            (int (Math/pow (* (pairs-count false true 0) (pairs-count true false 0)) (/ (- digits 3) 4))))))

(reduce + (map rev-count (range 10)))
