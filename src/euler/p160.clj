(ns euler.p160)

; appears to be a more efficient algo: http://www.mathpages.com/home/kmath489.htm

(def cap 1E12)

(def length 5)
(def d (int (Math/pow 10 length)))

(defn no-trailing-zeros [n]
  (if (zero? n)
    1
    (if (zero? (mod n 10))
      (recur (quot n 10))
      n)))

(defn last-digits [n]
  (let [a (mod n d)]
    (if (< a (/ d 10))
      (+ d a)
      a)))

(defn fac-factor-count [n]
  (loop [n1 n
         c 0N]
    (let [q (bigint (quot cap n1))]
      (if (zero? q)
        c
        (recur (* n1 n) (+ c q))))))

(defn truncate-multiply [a b]
  (last-digits (* a b)))

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

(def factor2-5
  (loop [f2 1
         nums (sorted-set)]
    (if (> f2 cap)
      nums
      (recur
        (* f2 2)
        (into nums
          (loop [f2-5 f2
                 nums [f2-5]]
            (if (> f2-5 cap)
              nums
              (recur (* f2-5 5) (conj nums f2-5)))))))))

(def last-digit-freq
  (loop [m {}
         n 2]
    (cond 
      (>= n d) m
      (or (zero? (mod n 2)) (zero? (mod n 5))) (recur m (inc n))
      :else (let [c (reduce +
                      (for [k factor2-5
                            :let [c (/ (- (/ cap k) n) d)]
                            :while (> c 0)]
                        (Math/ceil c)))]
              (recur (conj m [n c]) (inc n))))))

; take out factor 2s and 5s from n!
; find occurances of each possible trailing 5 digits
; rest of pow and multiplication can be efficiently calculated
(reduce
  (fn [r ld]
    (truncate-multiply r ld))
  (truncated-pow 2 (- (fac-factor-count 2) (fac-factor-count 5)))
  (map
    (fn [[ld c]]
      (truncated-pow ld c))
    last-digit-freq))
