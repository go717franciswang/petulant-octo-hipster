(ns euler.p140)

; ported from p137
; s(x) = 4 + 3x^2 + xs(x) + x^2s(x)
; => x = (-(1+s(x)) + sqrt((1+s(x))^2 + 4(3+s(x))s(x))) / (2(3+s(x)))
; => x is rational when (1+s(x))^2 + 4(3+s(x))s(x) is square

(defn square? [n]
  (let [a (Math/sqrt n)]
    (== a (int a))))

(defn rational-solutions? [n]
  (square? (+ (Math/pow (inc n) 2) (* 4 (+ 3 n) n))))

; from http://www.alpertron.com.ar/QUAD.HTM
(def diophantine-base-solutions
  [[0 -1] [0 1] [-3 -2] [-3 2] [-4 -5] [-4 5] [2 -7] [2 7]])

(def ss
  (reduce
    (fn [s solution]
      (let [[x0 y0] solution
            new-s (letfn [(x [n]
                           (if (zero? n)
                             x0
                             (+ (* -9 (x (dec n))) (* -4 (y (dec n))) -14)))
                          (y [n]
                            (if (zero? n)
                              y0
                              (+ (* -20 (x (dec n))) (* -9 (y (dec n))) -28)))]
                    (map x (range 1 15)))]
      (into s new-s)))
    []
    diophantine-base-solutions))

(reduce + (take 30 (sort (distinct (filter (partial < 0) ss)))))
