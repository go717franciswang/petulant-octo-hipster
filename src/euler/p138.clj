(ns euler.p138)

(defn int? [n]
  (== n (int n)))

(println 
  (loop [bh 2
         Ls []]
    (if (>= (count Ls) 4)
      Ls
      (recur (inc bh)
        (let [b (* 2 bh)
              h1 (dec b)
              h2 (inc b)
              L1 (Math/sqrt (+ (* bh bh) (* h1 h1)))
              L2 (Math/sqrt (+ (* bh bh) (* h2 h2)))
              L (first (filter int? [L1 L2]))]
          (if L
            (conj Ls L)
            Ls))))))

(defn a [n]
  "http://oeis.org/search?q=17%2C305%2C5473%2C98209&sort=&language=&go=Search"
  (cond
    (zero? n) 1
    (== n 1) 17
    :else (- (* 18 (a (dec n))) (a (- n 2)))))

(reduce + (map a (range 1 13)))
