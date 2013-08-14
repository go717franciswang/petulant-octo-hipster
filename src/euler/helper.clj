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
  (loop [r [[0 1] [1 n]]]
    (let [last-pos (count r)
          [[a b] [c d]] (subvec r (- last-pos 2) last-pos)]
      (if (> c n)
        (pop r)
        (let [k (int (/ (+ n b) d))]
          (recur (conj r [(- (* k c) a) (- (* k d) b)])))))))
