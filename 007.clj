(loop [primes (sorted-set 2)
       i 3]
  (if (= 10001 (count primes))
    (last primes)
    (if (some #(= 0 (mod i %)) primes)
      (recur primes (inc i))
      (recur (conj primes i) (inc i)))))
