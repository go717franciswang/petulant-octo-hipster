(ns euler.p108
  (:require euler.helper))

(def primes (euler.helper/primes 1000000))

(defn num-factors [n]
  (let [factors (loop [n n
                       primes primes
                       results []]
                  (if (= n 1)
                    results
                    (let [a (first primes)]
                      (if (zero? (mod n a))
                        (recur (/ n a) primes (conj results a))
                        (recur n (rest primes) results)))))]
    (reduce *
      (map 
        (fn [[_ factors]]
          (inc (count factors)))
        (group-by identity factors)))))

(defn num-solutions [n]
  (/ (inc (num-factors (* n n))) 2))

(first (filter 
         (fn [x]
           (let [a (num-solutions x)]
             (> a 1000)))
         (rest (range))))
