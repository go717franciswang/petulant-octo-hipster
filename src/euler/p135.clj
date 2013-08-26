(ns euler.p135)

; (z + 2i)^2 - (z + i)^2 - z^2 = n
; => (3i - z) * (i + z) = n
; let a * b = n, s.t. a = 3i - z, b = i + z
; => z = (3b - a) / 4, i = (b + a) / 4
; => unique positive solutions for z,i exist when 3b > a, 3b - a and b + a are divisible by 4

(def cap 1E6)

(time
  (count
    (filter #(= (second %) 10)
      (frequencies
        (for [a (range 1 cap)
              b (range)
              :let [n (* a b)]
              :while (< n cap)
              :when (and
                      (> (* 3 b) a) 
                      (zero? (mod (- (* 3 b) a) 4)) 
                      (zero? (mod (+ b a) 4)))]
          n)))))
