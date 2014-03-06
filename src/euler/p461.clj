(ns euler.p461)

; exhaustive search
; to increase the speed, create a BST with length = (dmax+1)^2
; to store f(0)+f(0), f(0)+f(1), ..., f(dmax)+f(dmax)

(def n 200)

(defn f [k]
  (dec (Math/exp (/ k n))))

(defn error [[a b c d]]
  (Math/abs (- (+ (f a) (f b) (f c) (f d)) Math/PI)))

(def dmax (int (* (Math/log (inc (- Math/PI (* (f 1) 3)))) n)))

(def cache (new java.util.TreeMap))
(doseq [i (range (inc dmax))
        j (range (inc dmax))]
  (.put cache (java.lang.Double. (+ (f i) (f j))) [i j]))










#_(loop [args [(* 6 50) (* 75 50) (* 89 50) (* 226 50)]
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

