(ns euler.p109)

(def region-2-score
  (reduce
    (fn [m i]
      (conj m [(str \S i) i] [(str \D i) (* 2 i)] [(str \T i) (* 3 i)]))
    {"S25" 25 "D25" 50 "MISS" 0}
    (range 1 21)))

(def regions (cons "MISS" (keys region-2-score)))

(def doubles-only (filter #(= (first %) \D) regions))

(def all-combos
  (distinct 
    (for [a regions
          b regions
          c doubles-only]
      [(sort [a b]) c])))

(defn score [[ab c]]
  (apply + (region-2-score c) (map region-2-score ab)))

(count (filter #(< (score %) 100) all-combos))
