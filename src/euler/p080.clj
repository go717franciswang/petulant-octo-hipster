(ns euler.p080)

; Babylonian method
; http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method

(def m (reduce * 1N (repeat 103 10)))

(def m-squared (* m m))

(defn first-100-digit-sum [n]
  (reduce +
    (map 
      (comp read-string str)
      ((comp (partial take 100) str bigint last (partial take 100))
        (iterate
          (fn [x]
            (* 1/2 (+ x (quot (* n m-squared) x))))
          (* (bigint (Math/pow n 0.5)) m))))))

(reduce +
  (map first-100-digit-sum
    (filter #(not= (Math/pow (int (Math/pow % 0.5)) 2) (float %)) (rest (range 1 101)))))
