(ns euler.p461)

; lazy approach: start at given abcd for 200, and pick a change 
; of either inc or dec 1 of one of abcd so that the error is minimized

(def n 10000)

(defn f [k]
  (dec (Math/exp (/ k n))))

(defn error [[a b c d]]
  (Math/abs (- (+ (f a) (f b) (f c) (f d)) Math/PI)))

(loop [args [(* 6 50) (* 75 50) (* 89 50) (* 226 50)]
       best (error args)]
  (println args)
  (let [[new-args new-best] (reduce (fn [[new-args new-best] [i o]]
                                      (let [try-args (update-in args [i] o)
                                            e (error try-args)]
                                        (println try-args e)
                                        (if (< e new-best)
                                          [try-args e]
                                          [new-args new-best])))
                                    [args best]
                                    (for [i (range 4)
                                          o [inc dec]]
                                      [i o]))]
    (if (= new-best best)
      (let [[a b c d] new-args]
        (+ (* a a) (* b b) (* c c) (* d d)))
      (recur new-args new-best))))

