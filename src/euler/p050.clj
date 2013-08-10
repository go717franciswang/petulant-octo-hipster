(ns euler.p050)

(def prime-cap 1000000)

(def primes 
  (loop [p (sorted-set 2)
         i 3]
    (if (> i prime-cap)
      p
      (let [sqrt (Math/pow i 0.5)]
        (if (some #(zero? (mod i %)) (take-while #(<= % sqrt) (rest p)))
          (recur p (+ i 2))
          (recur (conj p i) (+ i 2)))))))

(def primes-vec
  (vec primes))

(def prime-seq-sum 
  (memoize
    (fn [a b]
      (if (= a b)
        (nth primes-vec a)
        (+ (prime-seq-sum a (dec b)) (nth primes-vec b))))))

(reduce
  (fn [[max-length max-num] [l n]]
    (if (> l max-length)
      [l n]
      [max-length max-num]))
  (for [a (range (dec (count primes)))
        b (range (inc a) (count primes))
        :let [sum (prime-seq-sum a b)]
        :while (<= sum prime-cap)
        :when (contains? primes sum)]
    [(inc (- b a)) sum]))

