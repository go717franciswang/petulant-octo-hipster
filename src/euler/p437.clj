(ns euler.p437
  (:require [euler.helper :as h]))

(def cap 1E4)

(time (def primes (h/primes3 cap)))

(defn fib-primitive-root-bruteforce? [n]
  (loop [r 2]
    (if (>= r n)
      false
      (let [valid?  (loop [i 0
                           nums #{}
                           num-seq '()]
                      (let [x (if (== i 0)
                                1
                                (mod (* (first num-seq) r) n))
                            check-fib? (> (count num-seq) 2)
                            fib? (if check-fib?
                                   (let [[a b c] (take 3 num-seq)]
                                     ;(println r a b c)
                                     (== a (mod (+ b c) n)))
                                   true)]
                        (if (contains? nums x)
                          (and fib? (== (count nums) (dec n)))
                          (if fib?
                            (recur (inc i) (conj nums x) (conj num-seq x))
                            false))))]
        (if valid?
          true
          (recur (inc r)))))))

(defn z-eq? [n z]
  (loop [i 2
         a 1
         b 1]
    (if (== b 0)
      (== i z)
      (recur (inc i) b (int (mod (+ a b) n))))))

(defn qualified-prime? [n]
  (let [x (mod n 10)]
    (or (== x 1) (== x 9))))

; (def qualified-primes (int-array (filter qualified-prime? primes)))
; (println (vec qualified-primes))
; 
; (def q-count (count qualified-primes))
; 
; (time
; (loop [n 2
;        a 1N
;        b 1N]
;   (if (< n 1E5)
;     (recur (inc n) b (+ a b))
;     nil)))

; (def Z
;   (let [Z (int-array (count qualified-primes) 0)]
;     (loop [n 2
;            a 1N
;            b 1N]
;       (when (< n cap)
;         (loop [i 0]
;           (when (< i q-count)
;             (let [p (aget ^ints qualified-primes i)]
;               (when (< p n)
;                 (let [z (aget ^ints Z i)]
;                   (when (and (zero? z) (zero? (mod b p)))
;                     (aset ^ints Z i n)))
;                 (recur (inc i))))))
;         (recur (inc n) b (+ a b))))
;     Z))

            


(defn fib-primitive-root2? [n]
  (cond
    (let [x (mod n 20)] (or (== x 11) (== x 19))) (z-eq? n (dec n))
    (let [x (mod n 40)] (or (== x 1) (== x 9))) (z-eq? n (/ (dec n) 2))
    (let [x (mod n 40)] (or (== x 21) (== x 29))) (z-eq? n (/ (dec n) 4))
    :else false))

(defn xy [n]
  (loop [y 1]
    (let [yy5 (* 5 y y)
          xx (- n yy5)]
      (when (> xx 0)
        (let [x (Math/sqrt xx)]
          (if (== x (int x))
            [x y]
            (recur (inc y))))))))

(defn fib-primitive-root? [n]
  (cond
    (let [x (mod n 20)] (or (== x 11) (== x 19))) 
    (do (println (/ (dec n) 2))
      (.isProbablePrime (biginteger (/ (dec n) 2)) 15))
    (let [x (mod n 40)] (or (== x 1) (== x 9)))
      (if-let [[x y] (xy n)]
        (when (not (zero? (mod (* x y) 4)))
          ;true))
          (.isProbablePrime (biginteger (/ (dec n) 8)) 15)))
    (let [x (mod n 40)] (or (== x 21) (== x 29)))
      (if-let [[x y] (xy n)]
        (when (zero? (mod (* x y) 4))
          ;true))
          (.isProbablePrime (biginteger (/ (dec n) 4)) 15)))
    :else false))
 
; (time
; (reduce + 5 (filter fib-primitive-root? primes)))

(println (filter fib-primitive-root2? primes))
(time
(filter fib-primitive-root? primes))
