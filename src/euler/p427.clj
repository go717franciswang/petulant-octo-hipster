(ns euler.p427)

(declare F)

(defn f [m n k]
  ;number of permutations with length n (from m items) 
  ;exactly maximum of contiguous k-length"
  (cond
    (= n k) m
    (= k 1) (* m (Math/pow (dec m) (dec n)))
    :else (* m
             (reduce +
               (map 
                 (fn [i]
                   (* (max (F m (dec i) k) 1)
                      (if (> i 0) (dec m) 1)
                      (if (< i (- n k)) (dec m) 1)
                      (max (F m (- n k i 1) k) 1)))
                 (range (inc (- n k))))))))

(defn F [m n k]
  ;number of permutations with length n (from m items)
  ;no more than maximum of contiguous k-length"
  (if (or (< n 1) (< k 1))
    0
    (reduce + (map (partial f m n) (range 1 (inc k))))))

(println (reduce + (map #(* % (f 3 3 %)) (range 1 4))))

(println (reduce + (map #(* % (f (bigint 11) (bigint 11) %)) (range 1 12))))

(println (reduce + (map #(* % (f (bigint 7) (bigint 7) %)) (range 1 8))))

(println (reduce + (map (partial f 4 4) (range 1 5))))
(println (F 4 4 4))
(println (Math/pow 4 4))

;(reduce + 
        (map #(* 1 (f 4 4 %)) (range 1 5))
        ;)

