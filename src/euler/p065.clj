(ns euler.p065)

(def convergence-factors 
  (reverse
    (take 99
      (rest (interleave (iterate identity 1) (iterate identity 1) (map (partial * 2) (rest (range))))))))

(reduce +
  (map 
    #(read-string (str %))
    (str 
      (numerator
        (loop [factors (rest convergence-factors)
               result (first convergence-factors)]
          (if (empty? factors)
            (+ (/ 1 result) 2)
            (recur (rest factors) (+ (first factors) (/ 1 result)))))))))
