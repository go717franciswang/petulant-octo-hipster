(ns euler.p020)

(reduce 
  + 
  (map (comp read-string str) 
       (str (reduce 
              * 
              (map bigint (rest (range 101)))))))
