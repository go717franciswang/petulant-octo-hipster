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

; assume there can be exactly i elements > 1, it becomes a manageable combinatorial question
(def m (bigint 1E3))
(def modular 1234567891)

#_(def permute 
  (memoize
    (fn [element-count cap]
      (cond
        (zero? element-count) 1
        (= element-count 1) (- (bigint cap) 1)
        :else (loop [v 2
                     c 0]
                (let [new-cap (quot cap v)]
                  (if (< new-cap 1)
                    c
                    (recur (inc v) (bigint (+ c (permute (dec element-count) new-cap)))))))))))

#_(def permute 
  (memoize
    (fn [element-count cap]
      (cond
        (zero? element-count) 1
        (= element-count 1) (- (bigint cap) 1)
        (= element-count 2) (loop [v 2
                                   c 0]
                              (let [new-cap (quot cap v)]
                                (if (<= new-cap v)
                                  (+ (* c 2) (int (Math/sqrt cap)) -1)
                                  (recur (inc v) (+ c (- new-cap v))))))
        :else (loop [v 2
                     c 0]
                (let [new-cap (quot cap v)]
                  (if (< new-cap 1)
                    c
                    (recur (inc v) (bigint (+ c (permute (dec element-count) new-cap)))))))))))

(def permute 
  (memoize
    (fn [element-count cap floor]
      (cond
        (zero? element-count) 1
        (= element-count 1) (max (inc (- cap floor)) 0)
        :else (loop [v 2
                     c 0N]
                (let [new-cap (quot cap v)]
                  (if (<= v new-cap)
                    (bigint (+ (* c 2) (- (int (Math/sqrt cap)) 1)))
                    (recur 
                      (inc v) 
                      (bigint (+ c (permute (dec element-count) new-cap (inc v))))))))))))

(mod 
  (loop [i 0
         c 0N]
    (when (< i 20)
    (let [permutations (permute i m 2)]
      (if (zero? permutations)
        c
        (let [new-c (bigint (+ c (* permutations (h/C m i))))]
          (println i permutations new-c)
          (recur (inc i) new-c))))))
  modular)

#_(mod 
  (loop [i 0
         c 0N]
    (when (< i 20)
    (let [permutations (permute i m)]
      (if (zero? permutations)
        c
        (let [new-c (bigint (+ c (* permutations (h/C m i))))]
          (println i permutations new-c)
          (recur (inc i) new-c))))))
  modular)

