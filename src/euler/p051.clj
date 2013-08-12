(ns euler.p051)

(def check-digits 6)

(def check-digits-max-val (reduce * (repeat check-digits 10)))
(def check-digits-min-val (/ check-digits-max-val 10))

(def primes 
  (loop [p [2]
         i 3]
    (if (> i check-digits-max-val)
      (filter #(>= % check-digits-min-val) p)
      (let [sqrt (Math/pow i 0.5)]
        (if (some #(zero? (mod i %)) (take-while #(<= % sqrt) (rest p)))
          (recur p (+ i 2))
          (recur (conj p i) (+ i 2)))))))

(def ignore-digits (reduce * (repeat check-digits 2)))

(defn num-2-seq [n]
  (loop [r (list)
         n n]
    (if (< n 10)
      (conj r n)
      (recur (conj r (mod n 10)) (quot n 10)))))

(defn sub-num-seq [bits n]
  (let [s (num-2-seq n)
        replaced-vals (filter 
            identity 
            (map #(when (not (bit-test bits %2)) %) s (range)))]
    (when (and
            (not (empty? replaced-vals))
            (apply = replaced-vals))
      (map #(if (bit-test bits %2) % \*) s (range)))))

(reduce 
  (fn [[max-count max-list] [k v]]
    (let [c (count v)]
      (if (and k (> c max-count))
        [c v]
        [max-count max-list])))
  [0 []]
  (reduce
    merge
    (for [digits (range 1 ignore-digits)]
      (group-by (partial sub-num-seq digits) primes))))
