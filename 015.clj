(defn fact [n]
  (reduce * (map bigint (take n (rest (range))))))
(/ (fact (bigint 40)) (fact (bigint 20)) (fact (bigint 20)))
