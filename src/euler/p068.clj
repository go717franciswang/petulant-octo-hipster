(ns euler.p068)

(def s (set (range 1 11)))

(reduce
  max
  (map read-string
    (filter
      #(= (count %) 16)
      (map 
        #(apply str %)
        (for [a1 s
              :let [r1 (disj s a1)]
              a2 r1
              :let [r2 (disj r1 a2)]
              a3 r2
              :let [r3 (disj r2 a3)
                    b1 (+ a1 a2 a3)]
              a4 (disj s a1 a2 a3)
              :let [r4 (disj r3 a4)]
              a5 (disj s a1 a2 a3 a4)
              :let [r5 (disj r4 a5)
                    b4 (+ a4 a3 a5)]
              :when (= b1 b4)
              a6 r5
              :let [r6 (disj r5 a6)]
              a7 r6
              :let [r7 (disj r6 a7)
                    b6 (+ a6 a5 a7)]
              :when (= b4 b6)
              a8 r7
              :let [r8 (disj r7 a8)]
              a9 r8
              :let [r9 (disj r8 a9)
                    b8 (+ a8 a7 a9)]
              :when (= b6 b8)
              a10 r9
              :let [b10 (+ a10 a9 a2)]
              :when (= b10 b8)]
          (let [start-branch (first (sort-by first [[a1 0] [a4 1] [a6 2] [a8 3] [a10 4]]))
                branches (cycle
                           [a1 a2 a3
                            a4 a3 a5
                            a6 a5 a7
                            a8 a7 a9
                            a10 a9 a2])]
            (take 15 (drop (* (second start-branch) 3) branches))))))))




