(first
  (for [x (range 1 1000)
        y (range 1 1000)
        :let [z (- 1000 x y)]
        :when (and
                (> z 0)
                (= (+ (* x x) (* y y)) (* z z)))]
    (* x y z)))
