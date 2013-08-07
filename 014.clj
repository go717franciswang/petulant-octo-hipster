(defn collatz-seq-count 
  ([n c]
    (if (= 1 n)
      (inc c)
      (let [n2 (if (even? n)
                 (/ n 2)
                 (+ (* n 3) 1))]
        (recur n2 (inc c)))))
  ([n]
   (collatz-seq-count n 0)))
(def collatz-seq-count-memo (memoize collatz-seq-count))

(collatz-seq-count-memo 40)

(first 
  (reduce 
    (fn [[max_n max_c] [n c]]
      (if (> c max_c)
        [n c]
        [max_n max_c]))
    (for [x (rest (range 1000000))]
      [x (collatz-seq-count-memo x)])))
