(ns euler.p144)

(def start-pt [0.0 10.1])
(def second-pt [1.4 -9.6])

(defn subtract [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn scale [[x y] m]
  [(* x m) (* y m)])

(defn vector-from-pts [p1 p2]
  (subtract p2 p1))

(defn tangent-vector [[x y]]
  [y (* -4 x)])

(defn normal-vector [[x y]]
  [(- y) x])

(defn reflected-vector [v1 normal]
  (subtract normal v1))

(defn to-unit [[x y]]
  (let [l (Math/sqrt (+ (* x x) (* y y)))]
    [(/ x l) (/ y l)]))

(defn dot [[x1 y1] [x2 y2]]
  (+ (* x1 x2) (* y1 y2)))

(defn reflect [p1 p2]
  (let [d (vector-from-pts p1 p2)
        n (to-unit (normal-vector (tangent-vector p2)))]
    (to-unit (subtract d (scale n (* 2 (dot d n)))))))

(defn next-pt [p1 p2]
  (let [[ux uy :as u] (reflect p1 p2)
        [x y] p2
        a (+ (* 4 ux ux) (* uy uy))
        b (+ (* 8 x ux) (* 2 y uy))
        c (+ (* 4 x x) (* y y) -100)
        sqrt (Math/sqrt (- (* b b) (* 4 a c)))
        magnitude (/ (max (- sqrt b) (- (+ sqrt b))) (* 2 a))]
    (add p2 (scale u magnitude))))

(loop [p1 start-pt
       p2 second-pt
       i 0]
  (let [[x y :as p3] (next-pt p1 p2)]
    (if (and (>= x -0.01) (<= x 0.01) (> y 0))
      (inc i)
      (recur p2 p3 (inc i)))))
