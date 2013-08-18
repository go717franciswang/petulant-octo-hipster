(ns euler.p099)

(last
  (sort-by second
    (map-indexed
      (fn [idx exp-base-e]
        [(inc idx) exp-base-e])
      (map
        (fn [s]
          (let [[base exp] (clojure.string/split s #",")]
            (* (Math/log (read-string base)) (read-string exp))))
        (clojure.string/split
          (slurp (.getFile (clojure.java.io/resource "base_exp.txt")))
          #"\n")))))
