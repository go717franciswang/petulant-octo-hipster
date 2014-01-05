(ns euler.p451
  (:require [euler.helper :as h]))

; ; brute force
; (defn l [n]
;   (loop [x (- n 2)]
;     (if (or (= x 1) (= (mod (* x x) n) 1))
;       x
;       (recur (dec x)))))
; 
; ; get some test data
; (println (reduce + (pmap l (range 3 1001))))
; ; 3:1000 yield 278340

; is this called a sieve?
; let n be 2 : 2E7-2
;   let m = n*n-1
;   list all factors of m between n+2 and 2E7
;   update these numbers to use n as l(m)

(def cap 1000)

(def primes (h/primes cap))
(def L (int-array (inc cap) 1))

(defn prime-factors [n]
  (let [ceil (inc (int (Math/sqrt n)))]
    (loop [n n
           primes primes
           r {}]
      (let [p (first primes)]
        (cond
          (== n 1) r
          (> p ceil) (update-in r [n] (fnil inc 0))
          (zero? (mod n p)) (recur (/ n p) primes (update-in r [p] (fnil inc 0)))
          :else (recur n (rest primes) r))))))

(defn get-valid-factors [m minimum]
  (let [ps (prime-factors m)
        factors (reduce (fn [factors [p k]]
                          (let [multipliers (map #(int (Math/pow p %)) (range (inc k)))]
                            (for [f factors
                                  m multipliers]
                              (* f m))))
                        [1]
                        ps)]
    (take-while #(<= % cap) (drop-while #(< % minimum) factors))))

(loop [n 2]
  (when (<= n (- cap 2))
    (let [m (dec (* n n))
          factors (get-valid-factors m (+ n 2))]
      (doseq [x factors]
        (aset ^ints L x n)))
    (recur (inc n))))

(reduce + -3 L)

