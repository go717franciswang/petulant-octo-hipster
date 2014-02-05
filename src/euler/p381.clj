(ns euler.p381)

(defn factorial [n]
  (reduce * (range 1 (inc n))))

(defn S [p]
  (mod (reduce + (map factorial (range (- p 5) p))) p))

(map S (range 5 20))
