(ns euler.p120)

(defn pow [n m]
  (reduce * (bigint 1) (repeat m n)))

(defn rmax [n] 
  (let [square (* n n)]
    (if (even? n) 
      (- square (* 2 n))
      (- square n))))

(reduce + (map rmax (range 3 1001)))
