(ns euler.p082)

(def matrix
  (vec
    (map
      #(vec (map read-string (clojure.string/split % #",")))
      (clojure.string/split
        (slurp (.getFile (clojure.java.io/resource "matrix.txt")))
        #"\n"))))

(def inf (Double/POSITIVE_INFINITY))

(defn dijkstra [y]
  "find shortest path from position [y 0] to any cell in the last column"
  "we can assume first step always goes right (, not up or down)"
  (loop [visited #{[y 0] [y 1]}
         edges {[y 1] (+ (get-in matrix [y 0]) (get-in matrix [y 1]))}]
    (let [useless? (fn [[y1 x1]] (or (< y1 0) (> y1 79) (contains? visited [y1 x1])))
          neighbors (fn [y0 x0] [[(dec y0) x0] [(inc y0) x0] [y0 (inc x0)]])
          edges (reduce
                  (fn [m [[y0 x0] _]] 
                    (if (every? useless? (neighbors y0 x0)) 
                      (dissoc m [y0 x0])
                      m))
                  edges
                  edges) 
          [[y1 x1] d] (reduce
                        (fn [[mcell md] [cell d]]
                          (if (< d md)
                            [cell d]
                            [mcell md]))
                        (for [[[y0 x0] d0] edges
                              [y1 x1] [[(dec y0) x0] [(inc y0) x0] [y0 (inc x0)]]
                              :when (not (contains? visited [y1 x1]))
                              :let [d (+ d0 (get-in matrix [y1 x1] inf))]]
                          [[y1 x1] d]))]
      (if (= x1 79)
        d
        (recur (conj visited [y1 x1]) (assoc edges [y1 x1] d))))))

(reduce
  min
  (for [i (range 80)]
    (let [a (dijkstra i)]
      (println [i a])
      a)))

