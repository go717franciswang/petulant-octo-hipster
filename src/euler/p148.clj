(ns euler.p148)

; ; print out a nice picture of pascal triangle colored by divisibility
; ; for some insight
; (def pascals-triangle
;   (loop [t [[(bigint 1)]]]
;     (if (= (count t) 350)
;       t
;       (let [r (last t)
;             r2 (map + (concat [0] r) (concat r [0]))]
;         (recur (conj t r2))))))
; 
; (doseq [row pascals-triangle]
;   (let [pad (repeat (- (count pascals-triangle) (count row)) " ")
;         row (map 
;               (fn [n]
;                 (if (zero? (mod n 7)) "*" "."))
;               row)
;         line (apply str (take 180 (concat pad (interpose " " row))))]
;     (println line)))

(def cap 1E9)

(def count-map
  (loop [m (sorted-map 
             1 1 
             2 3 
             3 6 
             4 10
             5 15
             6 21
             7 28)]
    (let [[r c] (last m)]
      (if (> r cap)
        m
        (recur (conj m [(* r 7) (* c 28)]))))))

(defn indivisible-count [n]
  (let [[r c] (last (take-while #(<= (first %) n) count-map))
        t-rows (quot n r)
        t-count (/ (* (inc t-rows) t-rows) 2)
        t-remainder (inc t-rows)
        row-remainder (mod n r)]
    (if (zero? row-remainder)
      (* t-count c)
      (+ (* t-count c) (* t-remainder (indivisible-count row-remainder))))))

(indivisible-count (bigint 1E9))
