(reduce 
  + 
  (take-while 
    #(<= % 4000000) 
    (filter 
      even? 
      (map 
        second 
        (iterate (fn [[a b]] [b (+ a b)]) [1 1])))))
