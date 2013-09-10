(ns euler.p169)

(def cap 1E25)

; bruteforce to find first few results
; OEIS find Stern's diatomic series (http://oeis.org/A002487)
; (def elements
;   (loop [n 1N
;          r []]
;     (if (> n cap)
;       r
;       (recur (* n 2) (conj r n)))))
;
; (loop [m {}
;        elements elements]
;   ;(println (count elements) (count m) m)
;   (if (empty? elements)
;     ;(get m (bigint cap))
;     (map second (sort (filter #(> cap (first %)) m)))
;     (let [e (first elements)
;           e2 (+ e e)
;           ne (or (first (rest elements)) 0)
;           c (inc (get m e 0))
;           c2 (inc (get m e2 0))
;           m2 (conj m [e c] [e2 c2])
;           m (reduce
;               (fn [m [n c]]
;                 ;(if (even? n)
;                   (let [n1 (+ n e)
;                         n2 (+ n1 e)
;                         c1 (+ (get m n1 0) c)
;                         c2 (+ (get m n2 0) c)]
;                     (cond
;                       (> n1 cap) m
;                       (> n2 cap) (conj m [n1 c1])
;                       :else (conj m [n1 c1] [n2 c2]))))
;                   ;m))
;               m2
;               m)]
;       (recur m (rest elements)))))

(def a 
  (memoize
    (fn [n]
      (cond
        (== n 0) 0
        (== n 1) 1
        (even? n) (recur (/ n 2))
        :else (let [n0 (quot n 2)]
                (+ (a n0) (a (inc n0))))))))

(a (inc (reduce * (repeat 25 10N))))
