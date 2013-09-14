(ns euler.p215)

(def width 32)

(def combos 
  (memoize
    (fn [bricks height]
      (if (zero? height)
        1
        (let [old-bricks (set bricks)
              valid-next-levels (loop [new-bricks ['(2) '(3)]
                                       complete []]
                                  (if (empty? new-bricks)
                                    complete
                                    (let [new-bricks (filter
                                                       (fn [new-brick]
                                                         (not (contains? old-bricks (first new-brick))))
                                                       new-bricks)
                                          new-bricks (filter #(<= (first %) (- width 2)) new-bricks)
                                          new-complete (filter #(> (first %) (- width 4)) new-bricks)
                                          new-bricks (filter #(<= (first %) (- width 4)) new-bricks)
                                          new-bricks (for [brick new-bricks
                                                           add-brick [2 3]]
                                                       (conj brick (+ (first brick) add-brick)))]
                                      (recur new-bricks (into complete new-complete)))))]
          (reduce +
            (map #(combos % (dec height)) valid-next-levels)))))))

(doseq [height (range 1 11)]
  (println height (combos [] height)))



        


