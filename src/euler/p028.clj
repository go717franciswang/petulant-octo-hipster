(ns euler.p028)

(def incrementer 
  (cons 1 (flatten (map (partial repeat 4) (take-nth 2 (drop 2 (range)))))))

(defn num-seq [last-num incrementer]
  (if-let [next-num (+ last-num (first incrementer))]
    (lazy-seq (cons next-num (num-seq next-num (rest incrementer))))))

(reduce + (take (inc (* (/ (dec 1001) 2) 4)) (num-seq 0 incrementer)))

