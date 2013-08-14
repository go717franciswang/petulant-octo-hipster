(ns euler.p075)

; Euclid's formula 
; a = k(mm - nn), b = k2mn, c = k(mm + nn) => L = 2kmm + 2kmn => L = 2km(m + n)
; L with only one possible triple => L/2 is a product of 2 uniq numbers (either a prime or 1)
; while m > n

(def size 1500000)

(def primes (euler.helper/primes (int (/ size 4))))

(count
  (filter #(= 1 (second %))
    (frequencies
      (flatten 
        (for [m-and-n primes
              m (take-while (partial > m-and-n) primes)
              ; :let [a (- (* m m) (* n n))
              ;       b (* 2 m n)
              ;       c (+ (* m m) (* n n))]
              ; :while (< c 100)] 
              :let [l (* 2 m m-and-n)]
              :while (<= l size)]
          (let [max-k (int (/ size l))]
            (map (partial * l) (range 1 (inc max-k)))))))))

