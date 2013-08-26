(ns euler.p137)

(defn square? [n]
  (let [a (Math/sqrt n)]
    (== a (int a))))

(defn rational-solutions? [n]
  (square? (+ (* (inc n) (inc n)) (* 4 n n))))

(def phi (/ (inc (Math/sqrt 5)) 2))
  

(defn fib [n]
  (Math/round (/ (Math/pow phi n) (Math/sqrt 5))))

(* (fib 30) (fib 31))
