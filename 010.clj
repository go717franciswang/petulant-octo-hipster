(loop [primes [2]
       i 3]
  (if (>= i 2000000)
    (reduce + primes)
    (let [sqrt (Math/pow i 0.5)]
      (if (some #(zero? (mod i %)) (take-while #(<= % sqrt) (rest primes)))
        (recur primes (+ i 2))
        (recur (conj primes i) (+ i 2))))))
