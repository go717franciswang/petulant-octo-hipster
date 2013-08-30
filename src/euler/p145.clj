(ns euler.p145)

(def reversible-count 
  (memoize 
    (fn [digits remainder allow-zero?]
      (cond
        (= digits 1) (count (filter #(and (odd? %) (< % 10)) (map #(+ % % remainder) (range 10))))
        (= digits 2) (if (zero? remainder)
                       (let [start (if allow-zero? 0 1)]
                         (reduce +
                           (for [a (range start 10)
                                 b (range start 10)
                                 :when (and (odd? (+ remainder a b)) (< (+ a b) 10))]
                             1)))
                       0)
        :else (let [start (if allow-zero? 0 1)
                    valid-choices (for [a (range start 10)
                                        b (range start 10)
                                        :when (odd? (+ remainder a b))]
                                    (+ a b))
                    remainder-0 (count (filter #(< % 10) valid-choices))
                    remainder-1 (- (count valid-choices) remainder-0)
                    digits (- digits 2)]
                (+ (* remainder-0 (reversible-count digits 0 true))
                   (* 0 (reversible-count digits 1 true))))))))

(time
  (reduce + (map #(reversible-count % 0 false) (range 1 4))))

(defn rev? [n]
  (when (not= (mod n 10) 0)
    (let [r (read-string (apply str (map #(read-string (str %)) (reverse (str n)))))
          a (+ n r)]
      (every? odd? (map #(read-string (str %)) (str a))))))


(count (filter rev? (range 10000)))
;(+ 120 (reduce + (map #(reversible-count % 0 false) [4 6 8])))
(reversible-count 4 0 false)
