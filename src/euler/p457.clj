(ns euler.p457
  (:require [euler.helper :as h]))

; o = sqrt(13 + 4*p^2*q)
; q = (o^2 - 13) / (4*p^2)

(defn init [p]
  (let [init (int (Math/sqrt (+ (* 4 p p) 13)))]
    (if (even? init)
      (inc init)
      init)))

(defn R [p]
  (let [o (loop [o (init p)]
            (cond
              (> o (* p p)) nil
              (zero? (mod (- (* o o) 13) (* 4 p p))) o
              :else (recur (+ o 2))))]
    (when o
      (println [p o (/ o p) (float (/ o p))]))
    (if o
      (/ (+ 3 o) 2)
      0)))

(def primes (h/primes3 200))

(println (map R primes))

#_(let [p 7]
  (let [init (int (Math/sqrt (+ (* 4 p p) 13)))
        init (if (odd? init) init (inc init))]
    (doseq [o (range init (inc (* 2 p p)) 2)]
      (let [m (mod (- (* o o) 13) (* 4 p p))]
        (println o m)))))

