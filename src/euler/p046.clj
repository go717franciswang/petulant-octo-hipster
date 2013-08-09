(ns euler.p046)

(defn disprove? [primes n]
  (= :disproved
    (first
      (filter
        identity
        (map
          (fn [i]
            (let [check-n (- n (* 2 i i))]
              (if (< check-n 0)
                :disproved
                (if (contains? primes check-n)
                  :proved))))
          (rest (range)))))))

(loop [p (sorted-set 2)
       i 3]
  (let [sqrt (Math/pow i 0.5)]
    (if (some #(zero? (mod i %)) (take-while #(<= % sqrt) (rest p)))
      (if (disprove? p i)
        i
        (recur p (+ i 2)))
      (recur (conj p i) (+ i 2)))))

