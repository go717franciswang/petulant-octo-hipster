(ns euler.p126)

; get the following equation from observation
; n = 2(ab + bc + ac) + 4(l - 1)(a + b + c + l - 2)
; find the first n such that there exist 1000 integer solutions for [#{a b c} l]

; the brute force approach is generating a lot of solutions for a lot of cuboids

(def max-cuboid-size 10000)

(def n-cap 19000)

(defn cubes-required [a b c l]
  (+ (* 2 (+ (* a b) (* b c) (* a c))) (* 4 (dec l) (+ a b c l -2))))

(time
  (sort-by first
    (filter #(= (second %) 1000)
      (frequencies
        (for [a (range 1 (inc max-cuboid-size))
              b (range 1 (inc a))
              :while (<= (* 2 (+ (* a b) a b)) n-cap)
              c (range 1 (inc b))
              :while (<= (* 2 (+ (* a b) (* b c) (* a c))))
              l (rest (range))
              :let [cubes (cubes-required a b c l)]
              :while (<= cubes n-cap)]
          cubes)))))

