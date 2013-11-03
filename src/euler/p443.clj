(ns euler.p443
  (:require [euler.helper :as h]))

#_(loop [n 5
       g 13]
  (if (< n 1000000)
    (let [g2 (+ g (h/gcd n g))]
      (when (= (/ g2 n) 3)
        (println g [n g2]))
      (recur (inc n) (long g2)))
    g))

(def cap (bigint 1E15))

(defn sanity-check [n g tries]
  (if (zero? tries)
    true
    (if (= (h/gcd n g) 1)
      (recur (inc n) (inc g) (dec tries))
      false)))

; bruteforce?
(loop [n 5
       g 13]
  (if (< n cap)
    (let [g2 (+ g (h/gcd n g))]
      (if (and (= (/ g2 n) 3) (zero? (mod n 3)) (sanity-check (inc n) g2 10000000))
        (do
          (println [n g2])
          (recur (+ n (- n 2)) (+ g2 (- n 3))))
        (recur (inc n) (long g2))))
    (- g (- n cap) -1)))
