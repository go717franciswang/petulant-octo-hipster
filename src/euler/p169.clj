(ns euler.p169)

(def cap 1E25)

(def elements
  (loop [n 1N
         r []]
    (if (> n cap)
      r
      (recur (* n 2) (conj r n)))))

(loop [m {}
       elements (take 5 elements)]
  ;(println (count elements) (count m))
  (if (empty? elements)
    (get m (reduce * (repeat 25 10N)))
    (let [e (first elements)
          e2 (+ e e)
          ne (or (first (rest elements)) 0)
          c (inc (get m e 0))
          c2 (inc (get m e2 0))
          m2 (conj m [e c] [e2 c2])
          m (reduce
              (fn [m [n _]]
                (if (even? n)
                  (let [n1 (+ n e)
                        n2 (+ n1 e)
                        c1 (inc (get m n1 0))
                        c2 (inc (get m n2 0))]
                    (cond
                      (> n1 cap) m
                      (> n2 cap) (conj m [n1 c1])
                      :else (conj m [n1 c1] [n2 c2])))
                  m))
              m2
              m)]
      (recur m (rest elements)))))

; (def cumulative-sum
;   (zipmap
;     elements
;     (reduce
;       (fn [r n]
;         (conj r (+ (or (last r) 0) (* 2 n))))
;       []
;       elements)))
; 
; (loop [nums []
;        elements (reverse elements)
;        num-found 0]
;   (println (first elements) (count nums) num-found)
;   (if (empty? elements)
;     num-found
;     (let [n (first elements)
;           n1 (or (first (rest elements)) 0)
;           max-additional-sum (get cumulative-sum n1 0)
;           new-nums0 (filter #(and (>= (+ % max-additional-sum) cap) (<= % cap)) (conj nums n))
;           new-nums1 (filter #(and (>= (+ % max-additional-sum) cap) (<= % cap)) (map #(+ n %) nums))
;           new-nums2 (filter #(and (>= (+ % max-additional-sum) cap) (<= % cap)) (map #(+ n n %) nums))
;           [found1 nums1] (split-with (partial == cap) new-nums1)
;           [found2 nums2] (split-with (partial == cap) new-nums2)]
;       (recur (vec (concat new-nums0 nums1 nums2) )
;              (rest elements) 
;              (int (+ num-found (count found1) (count found2)))))))

