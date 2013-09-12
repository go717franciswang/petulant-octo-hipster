(ns euler.p225)

(defn valid? [n]
  (loop [a 1
         b 1
         c 1
         r #{}]
    (let [d (int (mod (+ a b c) n))]
      (if (zero? d)
        false
        (if (contains? r [a b c])
          true
          (recur b c d (conj r [a b c])))))))

(loop [n 27
       r []]
  (if (== (count r) 124)
    (last r)
    (if (valid? n)
      (recur (+ n 2) (conj r n))
      (recur (+ n 2) r))))
