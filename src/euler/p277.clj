(ns euler.p277
  (:require [euler.helper :as h]))

(def s (seq "UDDDUdddDDUDDddDdDddDDUDDdUUDd"))
(def floor 1E15)

; if number start at x, then after seq s, we end up with a1/a2*x + b1/a2
; => x = (n*a2 - b1) / a1 for some n that makes x an integer
(defn D [x1 x2]
  [(/ x1 3) (/ x2 3)])
(defn U [x1 x2]
  [(* x1 4/3) (/ (+ (* x2 4) 2) 3)])
(defn d [x1 x2]
  [(* x1 2/3) (/ (- (* x2 2) 1) 3)])

(loop [s s
       x1 1N
       x2 0N]
  (if (empty? s)
    (do
      (println [x1 x2])
      (let [a1 (numerator x1)
            a2 (denominator x1)
            b1 (numerator x2)]
        (loop [n 0N]
          (let [x (/ (- (* n a2) b1) a1)]
            (if (ratio? x)
              (recur (inc n))
              (do 
                (println x)
                (if (> x floor)
                  x
                  (recur (inc n)))))))))
    (let [call (first s)
          f (condp = call
              \D D
              \U U
              \d d)
          [x1 x2] (f x1 x2)]
      (recur (rest s) x1 x2))))
