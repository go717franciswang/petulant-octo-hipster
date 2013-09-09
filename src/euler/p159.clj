(ns euler.p159
  (:require [euler.helper :as h]))

(def cap 1E6)

(defn sum-digits [n]
  (loop [n n
         r 0]
    (if (> n 9)
      (recur (quot n 10) (int (+ r (mod n 10))))
      (+ r n))))

; ab = n
(defn digital-sum [n]
  (loop [n n]
    (let [n1 (sum-digits n)]
      (if (< n1 10)
        n1
        (recur n1)))))

(def primes (h/primes (inc (* 2 (Math/sqrt cap)))))

(defn prime-factors [n]
  (let [ceil (inc (int (Math/sqrt n)))]
    (loop [n n
           primes primes
           r {}]
      (let [p (first primes)]
        (cond
          (== n 1) r
          (> p ceil) (update-in r [n] (fnil inc 0))
          (zero? (mod n p)) (recur (/ n p) primes (update-in r [p] (fnil inc 0)))
          :else (recur n (rest primes) r))))))

(defn sub-sqrt-divisors [n]
  (let [factors (prime-factors n)
        ceil (inc (int (Math/sqrt n)))]
    (reduce
      (fn [divisors [p c]]
          (for [d divisors
                i (range (inc c))
                :let [d2 (* d (Math/pow p i))]
                :while (< d2 ceil)]
            (int d2)))
      [1]
      factors)))

(def max-digital-sum 
  (memoize
    (fn [n]
      (when (zero? (mod n 1E5)) (println n))
      (let [divisors (sub-sqrt-divisors n)]
        (int
          (reduce
            max
            (digital-sum n)
            (map
              (fn [d]
                (let [d2 (int (/ n d))]
                  (+ (max-digital-sum d) (max-digital-sum d2))))
              (rest divisors))))))))

(reduce + (map max-digital-sum (range 2 cap)))
