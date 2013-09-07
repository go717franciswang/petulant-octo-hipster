(ns euler.p156)

(defn f [n d]
  (loop [q 10
         r 0]
    (let [left (quot n q)
          right (mod n q)
          r (int (+ r 
                    (* left q 0.1)
                    (if (= q 10)
                      (if (>= right d) 1 0)
                      (max (- (min (inc right) (* (inc d) q 0.1)) (* d q 0.1)) 0))))]
      (if (zero? left)
        r
        (recur (* q 10) r)))))

(f 10 1)





