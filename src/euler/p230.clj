(ns euler.p230)

(def A "1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679")
(def B "8214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196")
(def max-n 17)

(defn f [n]
  (let [n (bigint n)]
    (* (+ 127 (* 19 n)) (reduce * (repeat n 7)))))

(def fib
  (memoize
    (fn [n]
      (cond
        (= n 1) (count A)
        (= n 2) (count B)
        :else (+ (fib (dec n)) (fib (- n 2)))))))

(defn min-iterations [n]
  (loop [i 1]
    (if (< (fib i) n)
      (recur (inc i))
      i)))

(defn lookup [i n]
  (cond
    (= i 1) (read-string (subs A (dec n) n))
    (= i 2) (read-string (subs B (dec n) n))
    :else (let [ia (- i 2)
                ib (- i 1)
                na (fib ia)
                nb (fib ib)]
            (if (>= na n)
              (recur ia n)
              (recur ib (- n na))))))

(reduce + 
  (for [i (range (inc max-n))
        :let [n (f i)]]
    (* (reduce * (repeat i 10N)) (lookup (min-iterations n) n))))



