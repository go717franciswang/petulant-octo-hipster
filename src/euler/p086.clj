(ns euler.p086
  (:require euler.helper
            [clojure.math.combinatorics :as combo]))

; brute force solution that takes hours

(def min-solutions 1000000)

(defn integer-shortest-path? [[a b c]]
  (let [z (reduce min
            (map (fn [[x y]] (Math/pow (+ (* x x) (* y y)) 0.5)) 
                 [[(+ a b) c] [(+ a c) b] [(+ b c) a]]))]
    (= (float (int z)) z)))

(defn triples-with-max [n]
  (map #(conj % n) 
    (concat
      (map #(list % %) (range 1 (inc n)))
      (combo/combinations (range 1 (inc n)) 2))))

(loop [M 1
       int-cuboids 0]
  (if (> int-cuboids min-solutions)
    (dec M)
    (recur
      (inc M)
      (let [c (+ int-cuboids (count (filter integer-shortest-path? (triples-with-max M))))]
        (println [M c])
        c))))

; ; use Farey series to generate coprimes [n m]
; ; then use Euclid's formula to generate Pythagorean triples
; ; then generate valid dimensions of cuboid room
; 
; (def min-solutions 2000)
; 
; (def M 100)
; 
; (count
;   (distinct
;     (apply concat
;       (for [[n m] (euler.helper/farey M)
;             :let [a (- (* m m) (* n n))
;                   b (* 2 m n)
;                   c (+ (* m m) (* n n))]
;             :when (and (<= a (* M 2)) (<= b (* M 2)))
;             k (rest (range))
;             :while (and (<= (* a k) (* M 2)) (<= (* b k) (* M 2)))]
;         (concat
;           (for [x (range 1 (inc (int (* a k 0.5))))
;                 :let [y (- a x)
;                       z (* b k)]
;                 :when (and (> y 0) (<= x M) (<= y M) (<= z M))]
;             (sort [x y z]))
;           (for [x (range 1 (inc (int (* b k 0.5))))
;                 :let [y (- b x)
;                       z (* a x)]
;                 :when (and (> y 0) (<= x M) (<= y M) (<= z M))]
;             (sort [x y z])))))))
