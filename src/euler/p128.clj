(ns euler.p128)

(def increments (map (partial * 6) (rest (range))))

; let l1 be the vertical line of hexagons containing 1
; l0 be the one on the left, l2 and l3 on the right
; observation shows that DP(n)=3 only occur aloing l1 and l2
(def l1 
  ((fn line [start increments] 
    (lazy-seq 
      (cons start (line (+ start (first increments)) (rest increments)))))
     2 increments))

(def l0 (rest (map inc l1)))

(def l2 (rest (map dec l1)))

(def l3 (rest (map dec l2)))

(def neighbors1 
  (map flatten 
    (partition 3 (interleave (partition 2 1 l0) (partition 3 1 l1) (partition 2 1 (rest l2))))))

(def neighbors2
  (map flatten
    (partition 3 (interleave (partition 2 1 l1) (partition 3 1 l2) (partition 2 1 l3)))))

(defn PD [n neighbors]
  (let [diffs (map #(biginteger (Math/abs (- n %))) neighbors)]
    (count (filter #(and 
                      (> % 1) 
                      (.isProbablePrime % 10)) 
                   diffs))))

(last (take 2000
  (list* 1 2
    (filter identity
      (map 
        (fn [n neighbors]
          (when (= (PD n neighbors) 3)
            n))
        (interleave (rest l1) (rest l2))
        (interleave neighbors1 neighbors2))))))
