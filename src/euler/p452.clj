(ns euler.p452
  (:require [euler.helper :as h]))

; ; check simple case F(10,10) = 571
; (def F 
;   (memoize
;     (fn [m n]
;       (cond
;         (= n 1) m
;         (= m 1) 1N
;         (< m 1) 0N
;         :else (loop [d 1
;                      sum 0N]
;                 (let [new-m (int (/ m d))]
;                   (if (< new-m 1)
;                     sum
;                     (recur (inc d) (bigint (+ sum (F new-m (dec n))))))))))))
; (println (F 10 10))

; assume there can be exactly i1 elements > 1, it becomes a manageable combinatorial question
; then assume there can be exactly i2 elemetns > 2, and so on.
; let F'(m,n,x) = count of n-tuples such that every element >= x
; so F(m,n) = F'(m,n,1)
; F'(m,n,x) = Sum(i=0 to m; C(m,i) * F'(floor(m/x^(n-i)), i, x+1))
; note had to treat n=0,1,2 as special case to avoid stack overflow

(def m (bigint 1E9))
(def modular 1234567891)

(def F' 
  (memoize
    (fn [m n x]
      (cond 
        (zero? n) 1
        (> (Math/pow x n) m) 0
        (= n 1) (inc (- m x))
        (= n 2) (loop [i 0
                       sum 0N]
                  (let [a (int (quot m (+ x i)))]
                    (if (< a (+ x i 1))
                      (+ (* 2 sum) (+ (- (int (Math/sqrt m)) x) 1))
                      (recur (inc i) (bigint (+ sum (- a (+ x i))))))))
        :else (inc (loop [i 1
                     c 0N]
                (if (> i n)
                  c
                  (let [a (F' (int (quot m (Math/pow x (- n i)))) i (inc x))]
                    (if (zero? a)
                      c
                      (recur (inc i) (bigint (+ c (* (h/C n i) a)))))))))))))

(time
  (mod (F' m m 1) modular))
