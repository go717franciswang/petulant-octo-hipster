(ns euler.p036)

(defn bin-palindrome? [n]
  (let [bin-seq (loop [n n
                       r (list)]
                  (cond
                    (>= n 2) (recur (quot n 2) (conj r (rem n 2)))
                    (= n 1) (conj r n)
                    :else r))
        l (int (/ (count bin-seq) 2))]
    (let [front (take l bin-seq)
          end (reverse (take-last l bin-seq))]
      (= front end))))

(defn seq-2-num [s]
  (loop [r 0
         s s]
    (if (empty? s)
      r
      (recur (+ (* 10 r) (first s)) (rest s)))))

(defn num-2-seq [n]
  (loop [r (list)
         n n]
    (if (< n 10)
      (conj r n)
      (recur (conj r (mod n 10)) (quot n 10)))))

(reduce
  +
  (filter
    #(and 
       (< % 1000000)
       (bin-palindrome? %)) 
    (for [middle (cons nil (map vector (range 10)))
          x (range 1000)
          :let [s (if (zero? x)
                    nil
                    (num-2-seq x))
                rs (reverse s)]]
      (seq-2-num (concat s middle rs)))))
