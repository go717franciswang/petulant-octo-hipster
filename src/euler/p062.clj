(ns euler.p062)

(def max-base 10000)

(def permutation-count 5)

(def cubes (drop-while #(< % 10000) (map #(* % % %) (range max-base))))

(map 
  #(reduce min (second %)) 
  (filter (comp (partial = permutation-count) count second) (group-by (comp sort str) cubes)))
