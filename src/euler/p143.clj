(ns euler.p143
  (:require [euler.helper :as h]))

(def cap 12E4)

(def cap-square (* cap cap))
(def cap-half (inc (int (* cap 0.5))))

(def pairs
  (for [m (range 2 (inc (int (Math/sqrt cap))));(/ (dec (int (Math/sqrt (inc (* 2 cap))))) 2))
        n (range 1 m)
        :when (and (not= 0 (mod (- m n) 3)) (= (h/gcd m n) 1))
        :let [r (- (* m m) (* n n))
              q (+ (* 2 m n) (* n n))
              a (+ (* m m) (* n n) (* m n))]
        k (range 1 (inc (int (/ cap (max r q)))))]
    [(* q k) (* r k) (* a k)]))

(def pairs-map
  (reduce
    (fn [m [q r a]]
      (assoc-in (assoc-in m [q r] a) [r q] a))
    {}
    pairs))

(time
  (reduce + 
    (distinct 
      (for [[r q2a] pairs-map
            [q a] q2a
            [p b] (get pairs-map q)
            :let [c (get-in pairs-map [p r])]
            :when (and c (<= (+ r q p) cap))]
        (+ r q p)))))

