(ns euler.p153
  (:require [euler.helper :as h]))

; product of conjugates always equal to an integer
; let primitive conjugates be a,b s.t a and b are coprime 
; all other conjugates can be obtained by scaling by k
; no product of two primitives can be an integer
; leaving us to look at only primitive conjugates
; since cap is 1E8, a^2 <= cap, it makes the search space quite small

(def cap 1E8)

(def appearances 
  (memoize
    (fn [max-k]
      (loop [n 1
             traceback '()
             result []]
        (let [q (quot max-k n)]
          (cond
            (> n max-k) (reduce
                          (fn [a [[from to] max-k]]
                            (+ a (/ (* (+ from to) (inc (- to from)) max-k) 2)))
                          0
                          result)
            (> q n) (recur (inc n) (conj traceback q) (conj result [[n n] q]))
            (= q n) (recur (inc n) traceback (conj result [[n n] q]))
            :else (let [to (int (first traceback))]
                    (recur (inc to) (rest traceback) (conj result [[n to] q])))))))))

(reduce +
  (for [a (range 1 (inc (int (Math/sqrt cap))))
        :let [aa (* a a)
              max-bb (- cap aa)]
        b (range (inc (int (Math/sqrt max-bb))))
        :when (== 1 (h/gcd a b))
        :let [max-k (int (/ cap (+ aa (* b b))))
              c (appearances max-k)]]
    (if (zero? b)
      (* a c)
      (* a c 2))))
