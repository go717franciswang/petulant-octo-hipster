(ns euler.p070)

(def size 10000000)

(def prime-size (int (* (Math/pow size 0.5) 2)))

(def primes 
  (loop [p [2]
         i 3]
    (if (> i prime-size)
      p
      (let [sqrt (Math/pow i 0.5)]
        (if (some #(zero? (mod i %)) (take-while #(<= % sqrt) (rest p)))
          (recur p (+ i 2))
          (recur (conj p i) (+ i 2)))))))

(defn prime-factors [n]
  (loop [n n
         ps primes
         result #{}]
    (if (= n 1)
      result
      (let [p (first ps)]
      (if (zero? (mod n p))
        (recur (/ n p) ps (conj result p))
        (recur n (rest ps) result))))))

(defn phi [n]
  "euler's totient product function"
  (loop [primes (prime-factors n)
         result n]
    (if (empty? primes)
      result
      (let [p (first primes)]
        (recur (rest primes) (* (/ result p) (dec p)))))))


(defn permutation? [a b]
  (= (sort (str a))
     (sort (str b))))

; n/phi(n) is minimized when it is a prime for large n
; however it must be product of many primes b/c n=prime cannot produce permutation of n
; let's assume the answer is product of 2 primes
(first
  (sort-by first
    (for [x primes
          y primes
          :let [n (* x y)
                phi (* (dec x) (dec y))]
          :when (and 
                  (< n size)
                  (not= x y)
                  (permutation? n phi))]
      [(float (/ n phi)) n])))
