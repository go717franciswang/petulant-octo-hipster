(ns euler.p058)

(defn prime? [n]
  (loop [checks (range 2 (+ 2 (int (Math/pow n 0.5))))]
    (cond
      (empty? checks) true
      (zero? (mod n (first checks))) false
      :else (recur (rest checks)))))

(def incrementer 
  (cons 1 (flatten (map (partial repeat 4) (take-nth 2 (drop 2 (range)))))))

(defn num-seq [last-num incrementer]
  (if-let [next-num (+ last-num (first incrementer))]
    (lazy-seq (cons next-num (num-seq next-num (rest incrementer))))))

(inc (* 
  2
  (loop [expansions (rest (num-seq 0 incrementer))
         total-count 1
         prime-count 0]
    (let [[spiral new-expansion] (split-at 4 expansions)
          new-prime-count (+ prime-count 
                             (count (filter prime? spiral)))
          new-total-count (+ 4 total-count)]
      (if (< (/ new-prime-count new-total-count) 0.1)
        (/ (dec new-total-count) 4)
        (recur new-expansion new-total-count new-prime-count))))))


