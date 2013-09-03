(ns euler.p152)

(def cap 45)

(loop [i 2
       sums #{}]
  (println i (count sums))
  (let [t (- 0.5 (reduce + (map #(* % %) (range (inc i) (inc cap)))))
        x (/ 1 (* i i))
        sums (set (filter #(and (> % t) (< % 0.5)) 
                     (concat sums (map (fn [n]
                                         (let [a (+ n x)]
                                           (when (== a 0.5)
                                             (println "found"))
                                           a)) 
                                       (conj sums 0)))))]
    (when (<= i cap)
      (recur (inc i) sums))))
