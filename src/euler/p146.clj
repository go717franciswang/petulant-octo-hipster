(ns euler.p146)

; n must end with 0 b/c ending with 1 to 9 will lead to a number divisible by 2 or 5

(defn give-consecutive-prime? [n]
  (when (not
          (or (zero? (mod n 3))
              (zero? (mod n 7))
              (zero? (mod n 13))))
    (let [sq (* n n)]
      (and (every? #(.isProbablePrime (biginteger (+ % sq)) 10) [1 3 7 9 13 27])
           (every? #(not (.isProbablePrime (biginteger (+ % sq)) 10)) [19 21])))))

(time 
  (reduce +
    (filter give-consecutive-prime? (range 10 150E6 10))))
