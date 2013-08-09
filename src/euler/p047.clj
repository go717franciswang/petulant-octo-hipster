(ns euler.p047)

(def consecutives 4)

(def prime-factors
  (memoize
    (fn 
      ([n d]
        (if (zero? (mod n d))
          (conj (prime-factors (/ n d) d) d)
          (if (> d (Math/pow n 0.5))
            (if (= n 1)
              (hash-set)
              (hash-set n))
            (recur n (inc d)))))
      ([n]
        (prime-factors n 2)))))

(prime-factors 8)

(loop [prime-factor-seq (map #(vector % (prime-factors %)) (rest (range)))]
  (let [check (take consecutives prime-factor-seq)]
    (if (every? #(= consecutives (count (second %))) check)
      (first (first check))
      (recur (rest prime-factor-seq)))))

