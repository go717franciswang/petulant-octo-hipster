(defn primes 
  ([n d]
   (cond
     (= n d) [n]
     (= 0 (mod n d)) (lazy-seq (cons d (primes (/ n d) d)))
     :else (primes n (inc d))))
  ([n]
   (primes n 2)))

(int (reduce
  (fn [r [k v]] (* r (Math/pow k v)))
  1
  (reduce 
    (fn [r [k v]]
      (if (< (get r k 0) v)
        (assoc r k v)
        r)) {} (reduce concat (map #(seq (frequencies (primes %))) (range 2 20))))))
