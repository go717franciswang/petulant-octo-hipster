(ns euler.p2970)

(def cap (bigint 1E17))
(def cap (bigint 1E6))

(defn fib [a b] (cons a (lazy-seq (fib b (+ b a)))))

(def fibs (vec (take-while #(< % cap) (fib 1N 2N))))

(def last-idx (count fibs))

(defn largest-fib-lte [n]
  (loop [i 0]
    (cond
      (>= i last-idx) (get fibs i)
      (> (get fibs (inc i)) n) (get fibs i)
      :else (recur (inc i)))))

(largest-fib-lte 21)

(defn z 
  ([n] (z n 0))
  ([n combos]
   (let [last-fib (largest-fib-lte n)]
     (if (= last-fib n)
       (inc combos)
       (recur (- n last-fib) (inc combos))))))

(loop [i 1
       sum 0]
  (when (< i 51)
    (println i (z i) (+ sum (z i)))
    (recur (inc i) (+ sum (z i)))))

; feed to oeis, and got http://oeis.org/A007895
; M(i) = M(i-1) . inc(M(i-2))
