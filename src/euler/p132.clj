(ns euler.p132
  (:require [euler.helper :as h]))

(defn A [n]
  (loop [k 1
         r 0]
    (let [r (mod (inc (* r 10)) n)]
      (if (zero? r)
        k
        (recur (inc k) r)))))

(def primes (h/primes 1E6))

(time
  (loop [primes (cons 3 (drop 3 primes))
         factors []]
    (if (= (count factors) 40)
      (reduce + factors)
      (let [p (first primes)
            a (A p)]
        (if (zero? (mod 1E9 a))
          (do
            (println p a)
            (recur (rest primes) (conj factors p)))
          (recur (rest primes) factors))))))


