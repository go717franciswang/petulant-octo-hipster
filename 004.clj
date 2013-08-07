(defn palindrome? [n]
  (let [h (/ n 2)
        s (seq (str n))]
    (= (take h s) (reverse (take-last h s)))))
(apply max 
       (filter 
         palindrome? 
         (for [x (range 100 999)
               y (range 100 999)]
           (* x y))))
