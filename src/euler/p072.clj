(ns euler.p072)

(def size 1000000)

(def primes (euler.helper/primes size))

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

(defn phi [n]
  "euler's totient product function"
  (loop [primes (prime-factors n)
         result n]
    (if (empty? primes)
      result
      (let [p (first primes)]
        (recur (rest primes) (* (/ result p) (dec p)))))))

(reduce + 
  (for [x (drop 2 (range (inc size)))]
    (phi x)))
