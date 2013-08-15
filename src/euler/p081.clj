(ns euler.p081)

; game rule states that D((0,0) -> (h,k)) = min(D((0,0) -> (h-1,k)), D((0,0) -> (h,k-1)))

(def matrix
  (vec
    (map
      #(vec (map read-string (clojure.string/split % #",")))
      (clojure.string/split
        (slurp (.getFile (clojure.java.io/resource "matrix.txt")))
        #"\n"))))

(def shortest-path 
  (memoize
    (fn [[y1 x1] [y2 x2]]
      (let [v2 (get-in matrix [y2 x2])
            d (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))]
        (cond 
          (= d 0) v2
          (= d 1) (+ (get-in matrix [y1 x1]) v2)
          (= y1 y2) (+ (shortest-path [y1 x1] [y2 (dec x2)]) v2)
          (= x1 x2) (+ (shortest-path [y1 x1] [(dec y2) x2]) v2)
          :else (+ v2
                  (min
                    (shortest-path [y1 x1] [y2 (dec x2)])
                    (shortest-path [y1 x1] [(dec y2) x2]))))))))

(doseq [y (range 80)
        x (range 80)]
  (shortest-path [0 0] [y x]))

(shortest-path [0 0] [79 79])
