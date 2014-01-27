(ns euler.p455
  (:require [euler.helper :as h]))

; https://oeis.org/search?q=411728896&language=english&go=Search

(def modulars
  (vec
    (for [i (range 1 12)]
      (biginteger (Math/pow 10 i)))))

(defn f [n]
  (if (zero? (mod n 10))
    0
    (let [n (biginteger n)]
      (loop [a n
             i 0]
        (let [new-a (.modPow n (biginteger a) (get modulars i))]
          (if (= i 10)
            (mod new-a (get modulars 8))
            (recur new-a (inc i))))))))

(time
  (loop [n 2
         sum (bigint 0)]
    (if (> n 1000000)
      sum
      (recur (inc n) (bigint (+ sum (f n)))))))


