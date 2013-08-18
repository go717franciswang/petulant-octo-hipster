(ns euler.p101)

(defn u [n]
  (bigdec
    (+ (- 1 n (Math/pow n 3) (Math/pow n 5) (Math/pow n 7) (Math/pow n 9))
       (Math/pow n 2) (Math/pow n 4) (Math/pow n 6) (Math/pow n 8) (Math/pow n 10))))

(def u-seq
  (vec (map u (range 1 20))))

(defn prepare-matrix [k]
  "a(0) + a(1)x^1 + .. + a(i)x^i + .. + a(n)x^k = y"
  (vec 
    (for [x (range 1 (inc k))]
      (let [coefficients (for [i (range k)] (Math/pow x i))]
        (conj (vec coefficients) (nth u-seq (dec x)))))))

(defn normalize-row [row offset]
  (let [unit (get row offset)]
    (vec (map #(/ % unit) row))))

(defn multiply-row [r1 m]
  (vec (map (partial * m) r1)))

(defn subtract-row [r1 r2]
  (vec (map - r1 r2)))

(defn reduce-matrix [matrix]
  (let [row-count (count matrix)]
    (reduce
      (fn [matrix offset]
        (let [row (get matrix offset)
              unit-row (normalize-row row offset)
              matrix (assoc matrix offset unit-row)]
          (reduce
            (fn [matrix o]
              (let [row (get matrix o)
                    m (get row offset)
                    new-row (subtract-row row (multiply-row unit-row m))]
                (assoc matrix o new-row)))
            matrix
            (filter #(not= offset %) (range row-count)))))
      matrix
      (range row-count))))

(defn OP [k]
  (let [coefficients (map last (reduce-matrix (prepare-matrix k)))]
    (fn [n]
      (loop [cs (reverse coefficients)
             r (bigdec 0)]
        (let [rn (* r n)
              c (bigdec (first cs))]
          (if (= 1 (count cs))
            (+ rn c)
            (recur (rest cs) (+ rn c))))))))

(defn FIT [k]
  (let [f (OP k)]
    (loop [n 1]
      (let [correct-num (get u-seq (dec n))
            op-num (f n)]
        (if (not= (bigint op-num) (bigint correct-num))
          (do
            (println "correct: " correct-num ", incorrect: " op-num)
            op-num)
          (recur (inc n)))))))

(reduce + (map FIT (range 1 11)))
