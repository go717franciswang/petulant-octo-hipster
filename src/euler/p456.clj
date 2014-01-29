(ns euler.p456
  (:require [euler.p102 :as e]))
  
(def cap 600)

(def points
  (let [a (biginteger 32323)
        b (biginteger 30103)]
    (loop [i 1
           points (transient [])]
      (if (> i cap)
        (persistent! points)
        (let [I (biginteger i)
              x (int (- (.modPow (biginteger 1248) I a) 16161))
              y (int (- (.modPow (biginteger 8421) I b) 15051))]
          (recur (inc i) (conj! points [x y])))))))

; brute-force, 8 minutes to compute C(600) hahaha
(defn C [n]
  (loop [i 0
         j 1
         k 2
         sum 0N
         total 0N]
    (cond
      (>= i n) [sum total]
      (>= j n) (recur (inc i) (+ i 2) (+ i 3) sum total)
      (>= k n) (recur i (inc j) (+ j 2) sum total)
      :else (let [a (get points i)
                  b (get points j)
                  c (get points k)]
              (if (e/origin-in-triangle? [a b c]) 
                (recur i j (inc k) (inc sum) (inc total))
                (recur i j (inc k) sum (inc total)))))))

(doseq [i (range 3 100)]
  (let [[sum total] (C i)]
    (println i (get points i) [sum total (float (/ sum total))])))
