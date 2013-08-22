(ns euler.p123
  (:require euler.helper))

(def primes (euler.helper/primes 1E6))

(defn r [p n]
  (rem (+ (.pow (biginteger (dec p)) n) (.pow (biginteger (inc p)) n)) (.pow (biginteger p) 2)))

(loop [n 1
       primes primes]
  (let [remainder (r (first primes) n)]
    (if (> remainder 1E10)
      n
      (recur (inc n) (rest primes)))))
