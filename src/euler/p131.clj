(ns euler.p131
  (:require [euler.helper :as h]))

; n^3 + p*n^2 = x^3
; => n^3 * (p+n) / n = x^3
; => p+n and n are cubes
; => p+n is capped by p+n < cap + ((p+n)^1/3 - 1)^3

(def cap 1E6)

(def primes (set (h/primes cap)))

(def cubes (map #(* % % %) (rest (range))))

(count
  (for [n-and-p (rest cubes)
        :while (<= n-and-p (+ cap (Math/pow (dec (Math/pow n-and-p 1/3)) 3)))
        n cubes
        :while (< n n-and-p)
        :let [p (- n-and-p n)]
        :when (contains? primes p)]
    p))

