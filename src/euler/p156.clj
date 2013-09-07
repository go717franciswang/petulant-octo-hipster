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

(defn valid-nums [a b d]
  (for [x (range a (inc b))
        :when (zero? (- (f x d) x))]
    x))
 
(defn s [d]
  (loop [searches [[1 1E11]]
         answers []]
    (if-let [[a b] (first searches)]
      (if (== (- b a) 1)
        (if (== (f a d) a)
          (recur (rest searches) (conj answers a))
          (recur (rest searches) answers))
        (let [m (quot (+ a b) 2)
              fm (f m d)
              fa (f a d)
              fb (f b d)
              ; there cannot be an answer when fm < a b/c no n b/w a and m can cross x=x
              ; similarly for fa > m
              search-lower? (and (>= fm a) (>= m fa))
              search-upper? (and (>= fb m) (>= b fm))
              more-searches (filter identity 
                                    [(if search-lower? [a m] nil)
                                     (if search-upper? [m b] nil)])]
          (recur (into (rest searches) more-searches) answers)))
      (reduce + answers))))

(bigint (reduce + (map s (range 1 10))))
