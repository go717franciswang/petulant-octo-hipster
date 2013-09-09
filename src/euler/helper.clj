(ns euler.helper)

(defn primes [n]
  "get primes up to n using Sieve of Eratosthenes"
  (let [limit (int (Math/pow n 0.5))
        half (int (/ n 2))
        x-multiples (fn [x upto]
                      (let [base (* x x)
                            t (take-while #(<= % n) (map #(+ base (* x %)) (range)))]
                        t))]
  (loop [sieve (transient (vec (interleave (repeat (inc half) false) (repeat half true))))
         x 3]
    (if (> x limit)
      (cons 2 (drop 1 (filter identity (map-indexed (fn [k v] (when v k)) (persistent! sieve)))))
      (if (get sieve x)
        (do 
          (recur 
            (reduce
              (fn [sieve i]
                (if (get sieve i)
                  (assoc! sieve i false)
                  sieve))
              sieve
              (x-multiples x n))
            (+ 2 x)))
        (recur sieve (+ 2 x)))))))

(defn primes2 [n]
  "prime1 using Java array"
  (let [limit (int (Math/pow n 0.5))
        half (int (/ n 2))
        sieve (boolean-array (interleave (repeat (inc half) false) (repeat half true)))]
    (doseq [x (range 3 limit 2)
            :when (aget sieve x)
            i (range (* x x) n x)]
      (aset-boolean sieve i false))
    (cons 2 (drop 1 (filter identity (map-indexed (fn [k v] (when v k)) (vec sieve)))))))

(defn primes3 [n]
  "ported from http://www.mathblog.dk"
  (let [limit (int (/ (dec (Math/sqrt n)) 2))
        sieve-bound (int (/ (dec n) 2))
        sieve (boolean-array (inc sieve-bound) true)]
    (loop [i 1
           ii 3
           iii 4]
      (when (<= i limit)
        (when (aget sieve i)
          (loop [j iii]
            (when (<= j sieve-bound)
              (aset-boolean sieve j false)
              (recur (+ j ii)))))
        (recur (inc i) (+ ii 2) (+ iii (* 4 (inc i))))))
    (loop [i 1
           ii 3
           result (transient [2])]
      (if (<= i sieve-bound)
        (if (aget sieve i)
          (recur (inc i) (+ ii 2) (conj! result ii))
          (recur (inc i) (+ ii 2) result))
        (persistent! result)))))

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

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn P [n x]
  (reduce * 1N (range (inc (- n x)) (inc n))))

(defn C [n x]
  (/ (P n x) (reduce * 1N (range 1 (inc x)))))

