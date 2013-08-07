(defn primes 
  ([n d]
   (cond
     (= n d) [n]
     (= 0 (mod n d)) (lazy-seq (cons d (primes (/ n d) d)))
     :else (primes n (inc d))))
  ([n]
   (primes n 2)))
(defn factor-count [n]
  (reduce * (map inc (vals (frequencies (primes n))))))
(defn nth-triangle-num [n]
  (/ (* (inc n) n) 2))

(first
  (first 
  (drop-while #(<= (second %) 500)
  (for [x (drop 500 (range))]
    (let [s (nth-triangle-num x)
          c (factor-count s)]
      [s c])))))
