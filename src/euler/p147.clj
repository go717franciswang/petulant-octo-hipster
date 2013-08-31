(ns euler.p147)

(defn rectangle-count [w h]
  (/ (* (inc h) h (inc w) w) 4))

(defn box-rows [w h]
  (let [w (dec w)
        h (dec h)]
    (loop [results []]
      (if (= (count results) (+ w h)) 
        results
        (let [[x y] (get results (dec (count results)) [0 0])
              x (cond
                  (< (count results) h) (dec x)
                  (= (count results) h) x
                  :else (inc x))
              y (cond
                  (< (count results) w) (inc y)
                  (= (count results) w) y
                  :else (dec y))]
          (recur (conj results [x y])))))))

(defn tilted-rectangles [w h]
  (let [rows (box-rows w h)]
    (reduce +
      (for [r (range (count rows))
            :let [row (get rows r)
                  end (second row)]
            start (apply range row)
            r2 (range r (count rows))
            :let [row2 (get rows r2)
                  [start2 end2] row2]
            :when (and (>= start start2) (> (min end end2) start))]
        (let [a (- (min end end2) start)]
          a)))))

(defn all-count [w h]
  (+ (rectangle-count w h)
     (tilted-rectangles w h)))

(reduce +
  (for [w (range 1 (inc 47))
        h (range 1 (inc 43))]
    (all-count w h)))

