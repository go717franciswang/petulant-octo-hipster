(ns euler.p427)

(declare F)

(defn f [n k]
  ;number of permutations with length n exactly maximum of contiguous k-length"
  (cond
    (= n k) n
    (= k 1) (* n (Math/pow (dec n) (dec n)))
    (or (<= n 0) (<= k 0)) 0
    :else (* n
             (reduce +
               (map 
                 (fn [i]
                   (* (max (F (dec i) (dec k)) 1)
                      (if (> i 0) (dec n) 1)
                      (if (< i (- n k)) (dec n) 1)
                      (max (F (- n k i 1) (dec k)) 1)))
                 (range (inc (- n k))))))))

(defn F [n k]
  ;number of permutations with no more than maximum of contiguous k-length"
  (if (or (<= n 0) (<= k 0))
    0
    (reduce + (map (partial f n) (range 1 (inc (- n k)))))))

(println (reduce + (map #(* % (f 3 %)) (range 1 4))))

(println (reduce + (map #(* % (f (bigint 11) %)) (range 1 12))))

(println (reduce + (map #(* % (f (bigint 7) %)) (range 1 8))))

;(reduce + 
        (map #(* % (f 7 %)) (range 1 8))
        ;)
