(ns euler.p030)

(reduce 
  + 
  (filter
    (fn [x]
      (= x (int (reduce + (map #(Math/pow (read-string (str %)) 5) (str x))))))
    (range 10 1000000)))
