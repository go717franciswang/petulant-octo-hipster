(ns euler.p069)

(def size 1000000)

(def primes 
  (loop [p [2]
         i 3]
    (if (> i size)
      (set p)
      (let [sqrt (Math/pow i 0.5)]
        (if (some #(zero? (mod i %)) (take-while #(<= % sqrt) (rest p)))
          (recur p (+ i 2))
          (recur (conj p i) (+ i 2)))))))

(def prime-factors
  (memoize
    (fn [n]
      (loop [n n
             ps primes
             result #{}]
        (if (= n 1)
          result
          (let [p (first ps)]
          (if (zero? (mod n p))
            (conj (prime-factors (/ n p)) p)
            (recur n (rest ps) result))))))))

; euler's totient function
(defn phi [n]
  (loop [primes (prime-factors n)
         result n]
    (if (empty? primes)
      result
      (let [p (first primes)]
        (recur (rest primes) (* (/ result p) (dec p)))))))

(reduce
  (fn [[mi mr] [i r]]
    (if (> r mr)
      [i r]
      [mi mr]))
  (map
    (fn [i] [i (/ i (phi i))]) 
    (rest (range size))))
