(ns euler.p216)

(loop [n 2N
       c 0]
  (if (> n 5E7)
    c
    (let [t (dec (* 2 n n))]
      (if (.isProbablePrime (biginteger t) 20)
        (recur (inc n) (inc c))
        (recur (inc n) c)))))

