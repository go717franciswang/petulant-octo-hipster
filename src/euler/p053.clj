(ns euler.p053)

(def fact 
  (memoize 
    (fn [n] (reduce * (bigint 1) (range 1 (inc n))))))

(defn choose [n r]
  (/ (fact n) (fact r) (fact (- n r))))

(count
  (filter
    #(> % 1000000)
    (for [n (range 1 101)
          r (range 1 n)]
      (choose n r))))
