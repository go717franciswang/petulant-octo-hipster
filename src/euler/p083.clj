(ns euler.p083)

(def matrix
  (vec
    (map
      #(vec (map read-string (clojure.string/split % #",")))
      (clojure.string/split
        (slurp (.getFile (clojure.java.io/resource "matrix.txt")))
        #"\n"))))

(def inf (Double/POSITIVE_INFINITY))

(defn dijkstra [y x]
  "find shortest path from position [y x] to [79 79]"
  (loop [visited #{[y x]}
         edges {[y x] (get-in matrix [y x])}]
    (let [useless? (fn [[y1 x1]] (or (< x1 0) 
                                     (> x1 79) 
                                     (< y1 0) 
                                     (> y1 79) 
                                     (contains? visited [y1 x1])))
          neighbors (fn [y0 x0] [[(dec y0) x0] 
                                 [(inc y0) x0] 
                                 [y0 (dec x0)]
                                 [y0 (inc x0)]])
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
                              [y1 x1] (neighbors y0 x0)
                              :when (not (contains? visited [y1 x1]))
                              :let [d (+ d0 (get-in matrix [y1 x1] inf))]]
                          [[y1 x1] d]))]
      (if (and (= x1 79) (= y1 79))
        d
        (recur (conj visited [y1 x1]) (assoc edges [y1 x1] d))))))

(dijkstra 0 0)
