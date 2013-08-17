(ns euler.p091)

; since there are less than 51^4 / 2 distinct triangles, let's brute force it

(def size 50)

(defn distance-sq [[x1 y1] [x2 y2]]
  (+ (Math/pow (- x1 x2) 2) (Math/pow (- y1 y2) 2)))

(defn right-triangle-by-points? [[p1 p2 p3]]
  (let [l1 (distance-sq p1 p2)
        l2 (distance-sq p1 p3)
        l3 (distance-sq p2 p3)
        [l1 l2 l3] (sort [l1 l2 l3])]
    (if (zero? l1)
      false
      (= (+ l1 l2) l3))))

(/ 
  (count
    (filter right-triangle-by-points?
      (for [x1 (range (inc size))
            y1 (range (inc size))
            x2 (range (inc size))
            y2 (range (inc size))]
        [[0 0] [x1 y1] [x2 y2]]))) 2)
