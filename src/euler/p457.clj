(ns euler.p457
  (:require [euler.helper :as h]))

; o = sqrt(13 + 4*p^2*q)
; q = (o^2 - 13) / (4*p^2)

(defn R [p]
  (let [o (loop [o 1]
            (cond
              (> o (* 2 p p)) nil
              (zero? (mod (- (* o o) 13) (* 4 p p))) o
              :else (recur (+ o 2))))]
    (when o
      (println [p o]))
    (if o
      (/ (+ 3 o) 2)
      0)))

(def primes (h/primes3 100))

(map R primes)

#_(doseq [o (range 1 (inc (* 2 p p)) 2)]
  (let [m (mod (- (* o o) 13) (* 4 p p))]
    (when (zero? m) 
      (println o m))))

