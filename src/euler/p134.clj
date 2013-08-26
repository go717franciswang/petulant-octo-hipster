(ns euler.p134
  (:require [euler.helper :as h]))

(def cap 1E6)

(def primes (h/primes (+ 1E2 cap)))

(def pairs (take-while #(< (first %) cap) (drop 2 (partition 2 1 primes))))

; for reference only
(defn S-bruteforce [[p1 p2]]
  (let [a (loop [i 1]
            (let [n (read-string (str i p1))]
              (if (zero? (mod n p2))
                n
                (recur (inc i)))))]
    (println [p1 p2] a)
    a))

(defn match-last-digit [last-digit m]
  (first (filter #(= (mod (* m %) 10) last-digit) (range 10))))

(defn S-long-division [[p1 p2]]
  (let [l (count (str p1))
        p2-last-digit (mod p2 10)
        m (loop [p1 p1
                 result-digits []]
            (if (= (count result-digits) l)
              (read-string (apply str (drop-while zero? (reverse result-digits))))
              (let [d (mod p1 10)
                    result-d (match-last-digit d p2-last-digit)]
                (recur (/ (- p1 (* p2 result-d)) 10) (conj result-digits result-d)))))]
    (* p2 m)))

(reduce + (map S-long-division pairs))
