(ns euler.p178)

(def cap 40)

(def step 
  (memoize
    (fn [steps-left dir up-allowed down-allowed min-reached max-reached]
      (if (zero? steps-left)
        (if (and (= min-reached 0) (= max-reached 9))
          1N
          0N)
        (reduce + 0N
          (if (= dir :up)
            (for [i (range 1 (inc (min up-allowed steps-left)))
                  :let [new-up-allowed (- up-allowed i)
                        pos (- 9 new-up-allowed)]]
              (step (- steps-left i) 
                    :dn
                    new-up-allowed 
                    (+ down-allowed i) 
                    (min pos min-reached) 
                    (max pos max-reached)))
            (for [i (range 1 (inc (min down-allowed steps-left)))
                  :let [new-up-allowed (+ up-allowed i)
                        pos (- 9 new-up-allowed)]]
              (step (- steps-left i) 
                    :up
                    new-up-allowed 
                    (- down-allowed i)
                    (min pos min-reached)
                    (max pos max-reached)))))))))

(defn S [n]
  (reduce + 
    (for [pos (range 1 10)]
      (let [a (+ (step (dec n) :up (- 9 pos) pos pos pos) 
                 (step (dec n) :dn (- 9 pos) pos pos pos))]
        ;(println n pos a)
        a))))

(reduce +
  (for [i (range 1 (inc cap))]
    (let [a (S i)]
      (println i a)
      a)))
