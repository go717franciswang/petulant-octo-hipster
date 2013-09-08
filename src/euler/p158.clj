(ns euler.p158)

(def monotics-seq-count 
  (memoize
    (fn [choices length]
      (if (== length 1)
        choices
        (reduce +
          (for [choice (range choices)]
            (monotics-seq-count (- choices choice 1) (dec length))))))))

; simply the problem by finding number of combinations of nCx
; within the set of x, we search for all valid sequence where only one lexigraphically greater
(defn C [n x]
  (/ (reduce * 1N (range (inc (- n x)) (inc n))) (reduce * 1N (range 1 (inc x)))))

; not an efficient implementation
; was lucky that the correct under came under 20 in length
; out of memory when tried 26 on old laptop
; (2^n - (n+1)) produces the correct result after reading thread
(defn seq-count [x]
  (let [partitions (loop [n 1
                          partitions [[[0] []]
                                      [[] [0]]]]
                     (if (== n x)
                       partitions
                       (recur 
                         (inc n)
                         (persistent!
                           (reduce 
                             (fn [partitions [a b]]
                               (conj! (conj! partitions [(conj a n) b]) [a (conj b n)]))
                             (transient [])
                             partitions)))))]
    (reduce +
      (map
        (fn [[left right :as pair]]
          (if 
            (some empty? pair) 0
            (if (> (last left) (first right)) 1 0)))
        partitions))))

(defn p [x]
  (* (C 26 x) (seq-count x)))

(reduce max
  (map p (range 1 20)))
