(ns euler.p187
  (:require [euler.helper :as h]))

(def cap 1E8)

(def primes (h/primes3 (int (/ cap 2))))

(loop [ps1 primes
       ps2 primes
       c 0]
  (if (empty? ps1)
    c
    (if (empty? ps2)
      (recur (rest ps1) (rest ps1) c)
      (let [p1 (first ps1)
            p2 (first ps2)
            p (* p1 p2)]
        (if (> p cap)
          (recur (rest ps1) (rest ps1) c)
          (recur ps1 (rest ps2) (inc c)))))))
