(ns euler.p025)

(def lowest-1000-digit-num
  (reduce * (repeat 999 (bigint 10))))

(+ 2 
  (count 
    (take-while #(< % lowest-1000-digit-num)
      (map 
        second 
        (iterate (fn [[a b]] [b (+ a b)]) [(bigint 1) 1])))))
