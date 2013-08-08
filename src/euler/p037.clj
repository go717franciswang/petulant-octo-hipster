(ns euler.p037)

(def primes 
  (set 
    (loop [p [2]
           i 3]
      (if (> i 1000000)
        (set p)
        (let [sqrt (Math/pow i 0.5)]
          (if (some #(zero? (mod i %)) (take-while #(<= % sqrt) (rest p)))
            (recur p (+ i 2))
            (recur (conj p i) (+ i 2))))))))

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

(defn truncatable? [prime]
  (let [digits (num-2-seq prime)
        truncates (map
                    seq-2-num
                    (concat
                      (take-while (comp not empty?) (iterate (partial drop-last 1) digits))
                      (take-while (comp not empty?) (iterate rest digits))))]
    (and
      (> (count digits) 1)
      (every? (partial contains? primes) truncates))))

(reduce + (filter truncatable? primes))
