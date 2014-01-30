(ns euler.p456
  (:require [euler.p102 :as e]))
  
(def cap 8)

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

#_(doseq [i (range 3 100)]
  (let [[sum total] (C i)]
    (println i (get points i) [sum total (float (/ sum total))])))

(def yx
  (vec (sort
    (map (fn [[x y]]
           (/ y x))
         points))))

(println yx)

(defn count-yx-lt [r]
  (loop [start 0
         end (dec cap)]
    (let [mid (quot (+ end start) 2)
          a (get yx (dec mid))
          b (get yx mid)]
      ;(println [start end mid a b])
      (cond 
        (nil? a) 0
        (= start end) (cond 
                        (< b r) cap
                        (<= a r) mid
                        :else (inc mid))
        (< b r) (recur (int (inc mid)) end)
        :else (recur start mid)))))
          
(loop [i 0
       sum 0N]
  (if (= i cap)
    (bigint (/ sum 3))
    (let [[x y] (get points i)
          r (/ x y)
          bc (- (count-yx-lt r) (if (< (/ y x) r) 1 0))]
      (println (str "r: " r ", bc: " bc ", half bc: " (quot bc 2)))
      (recur (inc i) (bigint (+ sum (* bc (dec bc) 0.5)))))))

(count-yx-lt -4170/5389)
