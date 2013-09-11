(ns euler.p206
  (:require [clojure.math.combinatorics :as combo]))

; 9_0 => end with 30
; 1a2b3c4d5e6f7g8h900

(def digits (map str (range 10)))
(def one-2-eight (map str (range 1 9)))

(defn square? [n]
  (let [a (Math/sqrt n)]
    (== a (bigint a))))

(loop [nums (apply combo/cartesian-product (repeat 8 digits))]
  (let [n (first nums)
        n1 (+ (* (read-string (apply str (interleave one-2-eight n))) 1000) 900)]
    (println n1)
    (if (square? n1)
      (Math/sqrt n1)
      (recur (rest nums)))))

; (for [a digits
;       b digits
;       c digits
;       d digits
;       e digits
;       f digits
;       g digits
;       h digits
;       :let [n (read-string (str "1" a "2" b "3" c "4" d "5" e "6" f "7" g "8" h "900"))
;             _ (println n)]
;       :when (square? n)]
;   (println n))
