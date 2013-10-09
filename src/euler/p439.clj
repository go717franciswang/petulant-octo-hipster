(ns euler.p439)

(def d 
  (memoize
    (fn [n]
      (reduce + 
        (for [i (range 1 (inc n))
              :when (zero? (mod n i))]
          i)))))

(defn S [n]
  (reduce + 
    (for [i (range 1 (inc n))
          j (range 1 (inc n))]
      (d (* i j)))))

(doseq [i (range 1 40)]
  (println i (S i)))
