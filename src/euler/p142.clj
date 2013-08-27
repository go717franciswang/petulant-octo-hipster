(ns euler.p142)

; x+y = a, x-y = b, ..
; => 2x = a+b = c+d, 2y = e+f = a-b; 2z = c-d = e-f
; => a > c > e > d > f > b
; => d = a-e, f = 2a-2c, b = c-e

(defn valid? [n]
  (and (> n 0) (== n (int n))))

(defn square? [n]
  (let [a (Math/sqrt n)]
    (== a (int a))))

(time
  (loop [a 6]
    (let [a2 (* a a)
          solutions (for [c (range 4 a)
                          :let [c2 (* c c)
                                f2 (- a2 c2)]
                          :when (square? f2)
                          e (range 3 c)
                          :let [e2 (* e e)
                                d2 (- a2 e2)
                                b2 (- c2 e2)]
                          :when (and (square? d2) (square? b2))
                          :let [x (/ (- (+ a2 c2) e2) 2.0)
                                y (/ (- (+ a2 e2) c2) 2.0)
                                z (/ (- (+ c2 e2) a2) 2.0)]
                          :when (every? valid? [x y z])]
                      (do
                        (println [a2 c2 e2 d2 f2 b2] [x y z])
                        (+ x y z)))]
      (if (empty? solutions)
        (recur (inc a))
        solutions))))

