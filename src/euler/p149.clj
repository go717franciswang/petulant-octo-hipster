(ns euler.p149)

(def size 2000)

(def nums
  (loop [k 1
         nums (transient [])]
    (cond 
      (> k 4E6) (vec (map vec (partition-all size (persistent! nums))))
      (<= k 55) (let [s (- (mod (+ (- 100003 (* 200003 k)) (* 300007 k k k)) 1E6) 5E5)]
                  (recur (inc k) (conj! nums s)))
      :else (let [s (- (mod (+ (get nums (- k 25)) (get nums (- k 56)) 1E6) 1E6) 5E5)]
              (recur (inc k) (conj! nums s))))))

(defn max-seq [nums]
  (let [l (count nums)]
    (println l)
    (loop [i 0
           max-sum 0
           connected-sums 0]
      (if (= i l)
        max-sum
        (let [n (get nums i)
              connected-sums (int (max connected-sums (+ connected-sums n)))
              max-sum (int (max max-sum connected-sums))]
          (recur (inc i) max-sum connected-sums))))))

; (def nums
;   [[0 1 2]
;    [3 4 5]
;    [6 7 8]])

(def cols (delay (apply mapv vector nums)))

(def diagonals
  (delay (cons 
    (vec (map #(get-in nums [% %]) (range size)))
    (apply concat 
      (for [i (range 1 (dec size))]
        (vector
          (vec (map #(get-in nums [% (+ i %)]) (range (- size i))))
          (vec (map #(get-in nums [(+ i %) %]) (range (- size i))))))))))

(def anti-diagonals
  (delay (cons
    (vec (map #(get-in nums [% (- (dec size) %)]) (range size)))
    (apply concat
      (for [i (range 1 (dec size))]
        (vector
          (vec (map #(get-in nums [(- (- size 2) %) %]) (range (- size i))))
          (vec (map #(get-in nums [(- (dec size) %) (inc %)]) (range (- size i))))))))))

(reduce max (map max-seq (lazy-cat nums @cols @diagonals @anti-diagonals)))
