(ns euler.p191)

(def combos 
  (memoize
    (fn [absents-allowed late-allowed? days]
      (cond
        (== days 0) 1
        late-allowed? (+ (combos 2 false (dec days))
                         (if (> absents-allowed 0) 
                           (+ (combos (dec absents-allowed) late-allowed? (dec days))
                              (combos 2 late-allowed? (dec days)))
                           (combos 2 late-allowed? (dec days))))
        :else (+ (if (> absents-allowed 0)
                   (+ (combos (dec absents-allowed) late-allowed? (dec days))
                      (combos 2 late-allowed? (dec days)))
                   (combos 2 late-allowed? (dec days))))))))

(doseq [i (range 30)]
  (println i (combos 2 true i)))

(combos 2 true 30)

