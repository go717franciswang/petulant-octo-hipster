(ns euler.p2970)

(def cap (dec (bigint 1E17)))

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
; observe pattern
; S1 1 : 1
; S2 2 : 1
; S3 3 : 12
; S4 5 : 122
; S5 8 : 12223
; S6 13: 12223233
; S7 21: 1222323323334
; S8 34: 122232332333423334344
; ...

(defn max-i [remainder C]
  (loop [i 0]
    (if (> (get C (inc i)) remainder)
      i
      (recur (inc i)))))

(time
  (loop [i 1
         C [1N 1N]
         S [1N 1N]
         T 2N]
    (let [new-T (+ T (+ (get C i) (get C (dec i))))]
      (if (> new-T cap)
        (let [S-sum (reduce + (take (inc i) S))]
          (loop [remainder (- cap T)
                 depth 0
                 additional-sum 0N]
            (let [i (max-i remainder C)
                  c (get C i)]
              (if (zero? remainder)
                (+ S-sum additional-sum)
                (recur (- remainder c) 
                       (inc depth) 
                       (+ additional-sum (get S i) (* c depth)))))))
        (recur (inc i)
               (conj C (+ (get C i) (get C (dec i))))
               (conj S (+ (get S i) (get S (dec i)) (get C (dec i))))
               (+ T (+ (get C i) (get C (dec i)))))))))

