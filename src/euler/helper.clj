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

(defn farey [n]
  "useful to generate coprimes"
  "http://en.wikipedia.org/wiki/Farey_sequence#Next_term"
  (let [f (fn f [[a b] [c d]]
            (if (> c n)
              '()
              (let [k (int (/ (+ n b) d))
                    next-term [(- (* k c) a) (- (* k d) b)]]
                (lazy-seq (cons [c d] (f [c d] next-term))))))]
    (cons [0 1] (f [0 1] [1 n]))))

(defn num-2-seq [n]
  (map #(read-string (str %)) (str n)))
