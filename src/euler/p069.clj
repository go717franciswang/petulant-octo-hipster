(ns euler.p069)

(def size 1000000)

(def primes 
  (loop [p [2]
         i 3]
    (if (> i size)
      p
      (let [sqrt (Math/pow i 0.5)]
        (if (some #(zero? (mod i %)) (take-while #(<= % sqrt) (rest p)))
          (recur p (+ i 2))
          (recur (conj p i) (+ i 2)))))))

; n / phi(n) = n / (n * product((p-1)/p)) = product(p/(p-1)) 
; and is maximized when every primes is used exactly once
(loop [primes primes
       r 1]
  (let [nr (* r (first primes))]
    (if (> nr 1000000)
      r
      (recur (rest primes) nr))))

