(ns euler.p078
  (:require euler.helper))

(def k-seq (interleave (rest (range)) (map - (rest (range)))))

; partition formula
(def p 
  (memoize 
    (fn [n]
      (cond
        (< n 0) 0
        (< n 2) 1
        :else (bigint
                (reduce +
                  (take-while #(not= % 0)
                    (map
                      (fn [k]
                        (let [sign (if (even? k) -1 1)
                              pentagonal (/ (* k (dec (* 3 k))) 2)]
                          (* sign (p (- n pentagonal)))))
                      k-seq))))))))

(time
  (loop [i 1]
    (let [a (p (bigint i))]
      (if (zero? (mod a divisible-by))
        [i a]
        (recur (inc i))))))
