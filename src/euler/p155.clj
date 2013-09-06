(ns euler.p155)

(defn parallel [c1 c2]
  (+ c1 c2))

(defn series [c1 c2]
  (/ 1 (+ (/ 1 c1) (/ 1 c2))))

(def capacity 60)

(def combinations 
  (memoize
    (fn [n]
      (if (= n 1) 
        #{capacity}
        (loop [n1 1
               combo #{}]
          (let [n2 (- n n1)
                combo1 (combinations n1)
                combo2 (combinations n2)
                combo (persistent! 
                        (reduce
                          (fn [combo [c1 c2]]
                            (conj! (conj! combo (parallel c1 c2)) (series c1 c2)))
                          (transient (clojure.set/union combo combo1 combo2))
                          (for [c1 combo1
                                c2 combo2]
                            [c1 c2])))]
            (if (>= n1 n2)
              combo
              (recur (inc n1) combo))))))))

(doseq [n (range 1 19)]
  (println n (count (combinations n))))
