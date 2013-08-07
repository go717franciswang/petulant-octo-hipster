(ns euler.p067)

(def triangle 
  (vec 
    (map #(vec (map (fn [s] 
                      (read-string
                        (if (= \0 (first s))
                          (str (second s))
                          s)))
                    (clojure.string/split (clojure.string/trim %) #"\s")))
      (clojure.string/split
        (slurp (.getFile (clojure.java.io/resource "triangle.txt"))) #"\n"))))

(def size (count triangle))

(def longest-path-len 
  (memoize
    (fn [x y]
      (let [v (get-in triangle [y x])]
        (if (= (inc y) size)
          v
          (+ v 
             (max (longest-path-len x (inc y))
                  (longest-path-len (inc x) (inc y)))))))))

(doseq [y (reverse (range size))
        :let [x-len (count (get triangle y))]]
  (doseq [x (range x-len)]
    (longest-path-len x y)))

(longest-path-len 0 0)
