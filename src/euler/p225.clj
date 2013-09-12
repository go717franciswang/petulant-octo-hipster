(ns euler.p225)

(defn valid? [n]
  (loop [a 1
         b 1
         c (int (mod 3 n))]
    (let [d (int (mod (+ a b c) n))]
      (if (zero? d)
        false
        (if (== a b c 1)
          true
          (recur b c d))))))

(loop [n 27
       r []]
  (if (== (count r) 124)
    (last r)
    (if (valid? n)
      (recur (+ n 2) (conj r n))
      (recur (+ n 2) r))))
