(ns euler.p183
  (:require [euler.helper :as h]))

(defn M [n]
  (let [a (/ n Math/E)]
    (Math/round a)))

(defn terminating? [n d]
  (let [g (h/gcd n d)
        d2 (/ d g)
        d3 (loop [d2 d2
                  tries [2 5]]
             (if (empty? tries)
               d2
               (let [t (first tries)]
                 (if (zero? (mod d2 t))
                   (recur (/ d2 t) tries)
                   (recur d2 (rest tries))))))]
    (== d3 1)))

(reduce
  (fn [sum n]
    (let [m (M n)
          t? (terminating? n m)]
      (+ sum (if t? (- n) n))))
  0
  (range 5 10001))
