(ns euler.p193
  (:require [euler.helper :as h]))

; http://oeis.org/A143658

; let p be primes
;   let pp = p*p
;   get the count of numbers below cap that are divisible by pp = cap/pp
;   remove the count of numbers that are divisible by other smaller pp
(def cap-pow-from-2 50)
(def cap (bigint (Math/pow 2 cap-pow-from-2)))
(def max-prime (int (Math/pow 2 (/ cap-pow-from-2 2))))
(def primes (h/primes3 (+ max-prime 100)))

; ; brute force for verification
; (defn square-free? [n]
;   (loop [x 2]
;     (let [xx (* x x)]
;       (cond
;         (> xx n) true
;         (zero? (mod n xx)) false
;         :else (recur (inc x))))))
; 
; (println (count (filter square-free? (range 1 (inc cap)))))

(defn inclusion-exclusion [i prod]
  (loop [i i
         sq 0N]
    (let [p (get primes i)
          q (* p p prod)]
      (if (> q cap)
        sq
        (recur 
          (inc i) 
          (bigint (+ sq (quot cap q) (* -1 (inclusion-exclusion (inc i) q)))))))))

(time
  (- cap (inclusion-exclusion 0 1N)))

