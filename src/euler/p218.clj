(ns euler.p218
  (:require [euler.helper :as h]))

(def cap 1E16)

(def max-m (int (Math/pow (/ cap 4) 0.25)))

(defn abs [n]
  (if (neg? n)
    (- n)
    n))

(loop [m 2N
       n 3N
       r 0]
  (if (> m max-m)
    r
    (if (= (h/gcd m n) 1)
      (let [a (- (* n n) (* m m))
            b (* 2 m n)
            c (+ (* m m) (* n n))
            a' (- (* a a) (* b b))
            b' (* 2 a b)
            c' (* c c)
            area (/ (* a' b') 2)]
        (cond
          (> c' cap) (do (println (inc m) r) (recur (inc m) (+ m 2) r))
          (and (not= 0 (mod area 84)) (= (h/gcd a' b') 1)) (do (println c') (recur m (inc n) (inc r)))
          :else (recur m (inc n) r)))
      (recur m (inc n) r))))
    
