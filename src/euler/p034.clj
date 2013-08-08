(ns euler.p034)

(defn num-2-seq [n]
  (loop [r (list)
         n n]
    (if (< n 10)
      (conj r n)
      (recur (conj r (mod n 10)) (quot n 10)))))

(def fact
  (memoize
    (fn [n]
      (if (< n 2)
        1
        (reduce * (range 2 (inc n)))))))

(defn curious? [n]
  (= n (reduce + (map fact (num-2-seq n)))))

(reduce + (filter curious? (range 10 2500000)))
