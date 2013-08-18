(ns euler.p102)

; copied from http://www.blackpawn.com/texts/pointinpoly/

(def triangles
  (map
    (fn [line]
      (let [[x0 y0 x1 y1 x2 y2] (map read-string (clojure.string/split line #","))]
        [[x0 y0] [x1 y1] [x2 y2]]))
    (clojure.string/split
      (slurp (.getFile (clojure.java.io/resource "triangles.txt")))
      #"\n")))

(defn sub [v1 v2]
  (vec (map - v1 v2)))

(defn dot [v1 v2]
  (reduce + (map * v1 v2)))

(defn barycentric-coor [a b c p]
  (let [v0 (sub c a)
        v1 (sub b a)
        v2 (sub p a)
        dot00 (dot v0 v0)
        dot01 (dot v0 v1)
        dot02 (dot v0 v2)
        dot11 (dot v1 v1)
        dot12 (dot v1 v2)
        inv-denom (/ 1.0 (- (* dot00 dot11) (* dot01 dot01)))
        u (* (- (* dot11 dot02) (* dot01 dot12)) inv-denom)
        v (* (- (* dot00 dot12) (* dot01 dot02)) inv-denom)]
    [u v]))

(defn origin-in-triangle? [[a b c]]
  (let [[u v] (barycentric-coor a b c [0 0])]
    (and (>= u 0) (>= v 0) (< (+ u v) 1))))

(time (count (filter origin-in-triangle? triangles)))
