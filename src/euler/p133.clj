(ns euler.p133
  (:require [euler.helper :as h]))

(defn A [n]
  (loop [k 1
         r 0]
    (let [r (mod (inc (* r 10)) n)]
      (if (zero? r)
        k
        (recur (inc k) r)))))

(defn divisible-by-pow-of-10? [n]
  (if (= n 1)
    true
    (if (zero? (mod n 2))
      (recur (quot n 2))
      (if (zero? (mod n 5))
        (recur (quot n 5))
        false))))

(def primes (h/primes (+ 1E4 1E5)))

(def prime-sum-divisible-by-pow-of-10
  (loop [primes (drop 4 primes)
         divisors []]
    (let [p (first primes)]
      (if (>= p 1E5)
        (reduce + divisors)
        (let [a (A p)]
          (if (divisible-by-pow-of-10? a)
            (do
              (println p a)
              (recur (rest primes) (conj divisors p)))
            (recur (rest primes) divisors)))))))

(def prime-sum
  (reduce + (take-while #(< % 1E5) primes)))

(- prime-sum prime-sum-divisible-by-pow-of-10)
