(ns euler.p267
  (:require [euler.helper :as h]))

(defn earning [f wins]
  (* (Math/pow (+ 1 (* 2 f)) wins) (Math/pow (- 1 f) (- 1000 wins))))

(defn min-wins [f]
  (loop [wins 1]
    (if (>= (earning f wins) 1E9)
      wins
      (recur (inc wins)))))

; shows that optimal f require at least 432 wins
#_(loop [a 0.0
       b 1.0]
  (let [e (/ (- b a) 3)
        y (+ a e)
        z (+ y e)
        yw (min-wins y)
        zw (min-wins z)]
    (println [y yw] [z zw])
    (cond
      (< yw zw) (recur a z)
      (> yw zw) (recur y b)
      (> (earning y yw) (earning z zw)) (recur a z)
      :else (recur y b))))

; TODO C can be computed recursively
(reduce +
  (for [wins (range 432 1001)]
    (* (h/C 1000 wins) (Math/pow 0.5 1000))))

