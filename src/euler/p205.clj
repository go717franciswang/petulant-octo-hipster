(ns euler.p205
  (:require [clojure.math.combinatorics :as combo]))

; monte carlo shows answer is around 0.5731
; (def pyramidal #(inc (rand-int 4)))
; (def cubic #(inc (rand-int 6)))
; 
; (defn simulate []
;   "return true if pete wins"
;   (> (reduce + (repeatedly 9 pyramidal))
;      (reduce + (repeatedly 6 cubic))))
; 
; (defn simulate-many [times]
;   (loop [i 0
;          w 0]
;     (if (> i times)
;       w
;       (recur (inc i) (if (simulate) (inc w) w)))))
; 
; (let [w (reduce + (pmap simulate-many (repeat 100 1000000)))]
;   (/ w 1E8))

(defn prob [rolls dice]
  (let [u (/ 1 (reduce * (repeat rolls (count dice))))]
    (reduce
      (fn [m n]
        (update-in m [n] (fnil #(+ % u) 0)))
      {}
      (map #(reduce + %) (apply combo/cartesian-product (repeat rolls dice))))))

(def prob-pete (prob 9 (range 1 5)))
(def prob-colin (prob 6 (range 1 7)))

(float
  (reduce +
    (for [a (range 9 37)
          b (range 6 a)
          :let [pp (prob-pete a)
                pc (prob-colin b)]]
      (* pp pc))))

