(ns euler.p156)

(defn diff-f [n1 n2 k]
  " = f(n2,k) - f(n1,k)"
  (let [digits2 (reverse (map #(read-string (str %)) (str n2)))
        l (count digits2)
        digits1 (reverse (map #(read-string (str %)) (str n1)))
        digit-diff (reduce + (map
                     (fn [d1 d2]
                       (if (and (< d1 k) (<= k d2)) 1 0))
                     (concat digits1 (repeat 0)) digits2))
        carry-diff (loop [digits1 digits1
                          digits2 digits2
                          i 1
                          r 0N]
                     (if (empty? digits2)
                       r
                       (recur (rest digits1)
                              (rest digits2)
                              (* i 10)
                              (+ r
                                (* i 
                                   (+
                                     (let [d1 (or (first digits1) 0)
                                           d2 (first digits2)]
                                       (if (and (< d1 k) (<= k d2)) 1 0))
                                     (- (if (empty? (rest digits2)) 0
                                          (read-string (apply str (reverse (rest digits2)))))
                                       (if (empty? (rest digits1)) 0 
                                         (read-string (apply str (reverse (rest digits1))))))))))))]
    (println digit-diff carry-diff)
    (+ 0 carry-diff)))

(diff-f 0 12 1)




