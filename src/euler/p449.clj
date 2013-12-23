(ns euler.p449)

; the shortest distance from the ellipse to the outer layer is formed by a projection
; that is perpendicular to the surface
; look at z-y plane, for any (z,y) on candy derive (z0,y0) on cholate 
; and do numerical integration

(def a 3)
(def b 1)
(def d 0.000001)

(def V
  (* 2
    (loop [z 0.0
           z0 0.0
           y0 (+ a 1.0)
           V 0.0]
      (if (<= z b)
        (let [y (Math/sqrt (* (- 1 (/ (* z z) b b)) a a))
              q (Math/pow (/ b a) 2)
              m (* -1 z a a (/ 1 b b) (Math/pow (* a a (- 1 (/ (* z z) b b))) -0.5))
              slope (/ -1 m)
              dz (Math/sqrt (/ 1 (+ 1 (* slope slope))))
              dy (if (zero? z) 1.0 (* slope dz))
              z1 (+ z dz)
              y1 (+ y dy)
              V (+ V (* Math/PI (+ (* y0 y0) (* y1 y1)) 0.5 (- z1 z0)))]
          (recur (+ z d) z1 y1 V))
        V))))

(- V (* 4/3 Math/PI a a b))
