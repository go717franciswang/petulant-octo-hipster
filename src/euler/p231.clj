(ns euler.p231
  (:require [euler.helper :as h]))

; nCk
(def n 2E7)
(def k 15E6)

(def primes (h/primes3 n))

(defn S [m]
  (loop [primes primes
         s 0N]
    (if (or (empty? primes) (> (first primes) m))
      s
      (let [p0 (first primes)
            s0 (loop [p p0
                      s 0N]
                 (let [s0 (* p0 (quot m p))]
                   (if (zero? s0)
                     s
                     (recur (* p p0) (bigint (+ s s0))))))]
        (recur (rest primes) (bigint (+ s s0)))))))

(- (S n) (S k) (S (- n k)))
