(ns euler.p156)

(defn f [n d]
  (loop [q 10
         r 0N]
    (let [left (quot n q)
          right (mod n q)
          r (bigint (+ r 
                    (* left q 0.1)
                    (if (= q 10)
                      (if (>= right d) 1 0)
                      (max (- (min (inc right) (* (inc d) q 0.1)) (* d q 0.1)) 0))))]
      (if (zero? left)
        r
        (recur (* q 10) r)))))

(loop [n 1]
  (when (< n 1E15)
    (println n (f n 1) (- n (f n 1)))
    (let [n2 (* 2 n)]
      (println n2 (f n2 1) (- n2 (f n2 1))))
    (recur (* n 10))))

(for [x (range 199981 200020)
      :let [a (f x 1)]
      :when (= x a)]
  (println x))





