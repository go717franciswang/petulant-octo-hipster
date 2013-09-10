(ns euler.p174)

(def cap 1E6)

(def tile-count (atom {}))

(defn laminae [start-size]
  (loop [l start-size
         c 0N]
    (if (<= l cap)
      (let [n (Math/floor (/ (- (Math/sqrt (+ (Math/pow (- l 4) 2) (* 16 cap))) l -4) 8))
            u (+ l (* 8 (dec n)))]
        (doseq [layer (range n)]
          (let [tiles (/ (* (+ l l (* 8 layer)) (inc layer)) 2)]
            ;(when (== tiles 32) (println l layer (+ l (* 8 layer))))
            (swap! tile-count update-in [tiles] (fnil inc 0))))
        (recur (+ l 8) (+ c (Math/round (inc (/ (- u l) 8))))))
      c)))

(println (+ (laminae 8) (laminae 12)))

(def a (group-by second @tile-count))
(println (count (get a 15)))

(reduce + (map #(count (get a % [])) (range 1 11)))
