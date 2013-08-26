(ns euler.p139
  (:require [euler.helper :as h]))

(def cap 1E8)

; use farey series to generate coprimes
; to generate primitive pythagorean triples
; to generate numbers of right triangles with perimeter less than 1E8
(time
  (reduce +
    (for [[n m] (rest (h/farey (int (inc (Math/sqrt (/ cap 2))))))
          :let [a (- (* m m) (* n n))
                b (* 2 m n)
                c (+ (* m m) (* n n))
                perim (+ a b c)]
          :when (and
                  (odd? (- m n))
                  (< perim cap)
                  (zero? (mod c (- b a))))]
      (int (/ (dec cap) perim)))))

