(ns euler.p449)

; general ellipsoid
; x^2/a'^2 + y^2/b'^2 + z^2/c'^2 = 1 => a'^2 = b'^2 = a^2*b, c'^2 = a*b^2
; => oblate ellipsoid (http://en.wikipedia.org/wiki/Oblate_spheroid)
; => Surface area = ...
; or calculate change in volume for adding that one uniform layer of chocolate?
; need to verify against test cases

; the shortest distance from the ellipse to the outer layer is formed by a projection
; that is perpendicular to the surface
; A = pi*a*b, V = 4/3*pi*a^2*b => A * 4/3 * a = V

(def a 2)
(def b 1)
(def d 0.00001)

(def V
  (* 4/3 (+ a 1) 4
    (loop [x 0.0
           x0 0.0
           y0 (+ a d)
           A 0.0]
      (if (<= x a)
        (let [y (Math/sqrt (* (- 1 (/ (* x x) a a)) b b))
              q (Math/pow (/ b a) 2)
              slope (/ -1 (* (Math/pow (- (* b b) (* q x x)) -0.5) -1 (* q x)))
              dx (Math/sqrt (/ 1 (+ 1 (* slope slope))))
              dy (if (zero? x) 1.0 (* slope dx))
              x1 (+ x dx)
              y1 (+ y dy)
              A (+ A (* (+ y0 y1) (- x1 x0) 0.5))]
          (recur (+ x d) x1 y1 A))
        A))))

#_(def V
  (* 2
    (loop [x 0.0
           x0 0.0
           y0 (+ a d)
           V 0.0]
      (if (<= x a)
        (let [y (Math/sqrt (* (- 1 (/ (* x x) a a)) b b))
              q (Math/pow (/ b a) 2)
              slope (/ -1 (* (Math/pow (- (* b b) (* q x x)) -0.5) -1 (* q x)))
              dx (Math/sqrt (/ 1 (+ 1 (* slope slope))))
              dy (if (zero? x) 1.0 (* slope dx))
              x1 (+ x dx)
              y1 (+ y dy)
              V (+ V (* (+ (* Math/PI y0 y0) (* Math/PI y1 y1)) (- x1 x0) 0.5))]
          (recur (+ x d) x1 y1 V))
        V))))

(- V (* 4/3 Math/PI a a b))
