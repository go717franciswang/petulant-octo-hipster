(ns euler.p041)

(defn pandigital? [n]
  (let [s (str n)]
    (= (set s) (set (apply str (range 1 (inc (count s))))))))

(def primes 
  (set 
    (loop [p [2]
           i 3]
      (if (> i 10000000)
        p
        (let [sqrt (Math/pow i 0.5)]
          (if (some #(zero? (mod i %)) (take-while #(<= % sqrt) (rest p)))
            (recur p (+ i 2))
            (recur (conj p i) (+ i 2))))))))

(last (sort (filter pandigital? (reverse primes))))

