(ns euler.helper)

(defn primes [n]
  "get primes up to n using Sieve of Eratosthenes"
  (let [limit (int (Math/pow n 0.5))
        half (int (/ n 2))
        x-multiples (fn [x upto]
                      (let [base (* x x)
                            t (take-while #(<= % n) (map #(+ base (* x %)) (range)))]
                        t))]
  (loop [sieve (vec (interleave (repeat (inc half) false) (repeat half true)))
         x 3]
    (if (> x limit)
      (cons 2 (drop 1 (filter identity (map-indexed (fn [k v] (when v k)) sieve))))
      (if (get sieve x)
        (do 
          (recur 
            (reduce
              (fn [sieve i]
                (if (get sieve i)
                  (assoc sieve i false)
                  sieve))
              sieve
              (x-multiples x n))
            (+ 2 x)))
        (recur sieve (+ 2 x)))))))
