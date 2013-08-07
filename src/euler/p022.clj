(ns euler.p022)

(defn string-val [s]
  (reduce + (map #(- (int %) 64) s)))

(reduce 
  +
  (map-indexed
    (fn [idx itm]
      (* (inc idx) (string-val itm)))
    (sort
      (map 
        #(subs % 1 (dec (count %))) 
        (clojure.string/split 
          (slurp (.getFile (clojure.java.io/resource "names.txt")))
          #",")))))
