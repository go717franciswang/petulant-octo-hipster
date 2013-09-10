(ns euler.p170)

; use Lagrange (http://en.wikipedia.org/wiki/Lagrange_multiplier)
; to find x_2 = 2x_1, ..., (m-1)x_m = mx_m-1, s.t. sum(x_i) = m
; => x_i = ix_1 => x_i = 2i/(1+m)

(defn P [m]
  (reduce
    (fn [p i]
      (* p (Math/pow (/ (* 2 i) (inc m)) i)))
    1
    (range 1 (inc m))))

(reduce +
  (map (comp int P) (range 2 16)))
      
