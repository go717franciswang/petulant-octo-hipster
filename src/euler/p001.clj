(reduce (fn [r x]
          (if (or
                (= 0 (mod x 3))
                (= 0 (mod x 5)))
            (+ r x)
            r))
        (range 0 1000))
