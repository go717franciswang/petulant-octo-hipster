(ns euler.p150)

(def bound (Math/pow 2 20))
(def offset (Math/pow 2 19))
(def rows 1000)

(def s
  (map 
    #(- % offset)
    (take (/ (* rows (inc rows)) 2) (rest
      (iterate 
        (fn [t]
          (mod (+ (* 615949 t) 797807) bound))
        0)))))

(def t
  (loop [s s
         t (transient [])]
    (if (empty? s)
      (persistent! t)
      (let [n (inc (count t))]
        (recur
          (drop n s) 
          (conj! t (vec (take n s))))))))

(defn step-vals [row]
  (persistent!
    (reduce 
      (fn [r n]
        (let [l (count r)
              a (+ (get r (dec l) 0) n)]
          (conj! r a)))
      (transient [])
      row)))

(defn conneted-partition-sum [row]
  (let [l (count row)
        steps (step-vals row)]
    (for [size (range l 1 -1)
          i (range 0 (inc (- l size)))
          :let [k (dec (+ i size))]]
      (- (get steps k) (get steps (dec i) 0)))))

(loop [rows t
       connected-sum []
       min-sum 0]
  (println (count rows) min-sum)
  (if (empty? rows)
    min-sum
    (let [row (first rows)
          connected-sum (concat (map + 
                                     connected-sum 
                                     (conneted-partition-sum row)) row)
          min-sum (int (reduce min min-sum connected-sum))]
      (recur (rest rows) connected-sum min-sum))))

