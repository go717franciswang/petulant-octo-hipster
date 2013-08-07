(reduce + (map (comp read-string str) (str (reduce * (map bigint (repeat 1000 2))))))
