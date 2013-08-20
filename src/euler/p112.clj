(ns euler.p112)

(defn bouncy? [n]
  (let [s (seq (str n))
        ss (sort s)]
    (and (not= s ss)
         (not= s (reverse ss)))))

(loop [n 0
       bc 0]
  (let [n (inc n)
        bc (+ bc (if (bouncy? n) 1 0))]
    (if (= (/ bc n) 99/100)
      n
      (recur n bc))))
