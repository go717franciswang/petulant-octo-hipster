(ns euler.p160)

; cap = 5 => 2496
; cap = 6 => 4544
; cap = 7 => 51552 from bruteforce

(def cap 1E5)

(def length 5)
(def d (int (Math/pow 10 5)))

(defn no-trailing-zeros [n]
  (if (zero? n)
    1
    (if (zero? (mod n 10))
      (recur (quot n 10))
      n)))

(defn last-digits [n]
  (mod n d))

(defn fac-factor-count [n]
  (loop [n1 n
         c 0N]
    (let [q (bigint (quot cap n1))]
      (if (zero? q)
        c
        (recur (* n1 n) (+ c q))))))

(defn truncate-multiply [a b]
  (last-digits (* a b)))

; (println 
; (loop [n 1
;        r 1N]
;   (if (== n 1E6)
;     r
;     (recur (inc n) (last-digits (no-trailing-zeros (* r n)))))))

(defn truncated-pow [x n]
  (loop [m {1 x}
         s [1]
         n1 1N
         r x]
    (let [n0 (last s)
          n2 (bigint (+ n0 n1))
          x0 (get m n0)
          x2 (truncate-multiply r x0)]
      (cond 
        (== n1 n) r
        (<= n2 n) (recur (conj m [n2 x2])
                         (if (== (* n1 2) n2) (conj s n2) s)
                         n2
                         x2)
        :else (recur m (butlast s) n1 r)))))

(defn factor-count [n p]
  (loop [n n
         c 0]
    (if (zero? (mod n p))
      (recur (/ n p) (inc c))
      c)))

(def last-digit-freq
  (loop [m {}
         n 1]
    (let [c2 (factor-count n 2)
          c5 (factor-count n 5)
          n0 (int (/ n (Math/pow 2 c2) (Math/pow 5 c5)))]
      (if (< n 1E5)
        (recur (update-in m [n0] 
                          (fnil inc 0N)) 
               (inc n))
        m))))

(println (take 20 last-digit-freq))

(reduce
  (fn [r ld]
    (truncate-multiply r ld))
    (truncated-pow 2 (- (fac-factor-count 2) (fac-factor-count 5)))
  (map
    (fn [[ld c]]
      (truncated-pow ld c))
    last-digit-freq))


