(ns euler.p130
  (:require [euler.helper :as h]))

(defn A [n]
  (loop [k 1
         r 0]
    (let [r (mod (inc (* r 10)) n)]
      (if (zero? r)
        k
        (recur (inc k) r)))))

(def primes (set (h/primes 1E5)))

(loop [n 707
       nums [91 259 451 481 703]]
  (if (= (count nums) 25)
    (reduce + nums)
    (if (contains? primes n)
      (recur (+ n 2) nums)
      (if (and (not= (mod n 5) 0) (zero? (mod (dec n) (A n))))
        (do (println n)
          (recur (+ n 2) (conj nums n)))
        (recur (+ n 2) nums)))))

