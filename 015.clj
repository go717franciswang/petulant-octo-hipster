(defn fact [n]
  (reduce * (map bigint (take n (rest (range))))))
(/ (fact 40) (fact 20) (fact 20))
