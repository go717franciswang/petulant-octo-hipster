(ns euler.p143
  (:require [euler.helper :as h]))

(def cap 12E4)

(def cap-square (* cap cap))
(def cap-half (inc (int (* cap 0.5))))
(def cap-half-square (* cap-half cap-half))

(defn int? [n]
  (== n (int n)))

(time
(reduce +
(for [[n m] (rest (h/farey (inc (int (/ (dec (int (Math/sqrt (inc (* 2 cap))))) 2)))))
      :let [r (- (* m m) (* n n))
            q (+ (* 2 m n) (* n n))
            a (+ (* m m) (* n n) (* m n))]
      p (range 1 q)
      :let [b (Math/sqrt (+ (* p p) (* r r) (* p r)))
            c (Math/sqrt (+ (* p p) (* q q) (* p q)))
            perim (+ a b c)]
      :when (and (int? b) (int? c))
      k (range 1 (inc (int (/ cap perim))))]
  (* perim k))))

      

; (time
;   (reduce +
;     (for [r (range 3 (inc cap-half))
;           :let [r2 (* r r)]
;           q (range 2 r)
;           :let [q2 (* q q)
;                 a2 (+ r2 q2 (* r q))]
;           :while (< a2 cap-half-square)
;           :let [a (Math/sqrt a2)]
;           :when (int? a)
;           p (range 1 q)
;           :let [p2 (* p p)
;                 b2 (+ q2 p2 (* q p))
;                 b (Math/sqrt b2)]
;           :when (int? b)
;           :let [c2 (+ r2 p2 (* r p))
;                 c (Math/sqrt c2)]
;           :when (int? c)
;           :let [perim (+ a b c)]
;           :when (<= perim cap)]
;       (+ r q p))))
; 
; (time
;   (loop [r 3
;          results (transient [])]
;     (if (> r cap-half)
;       (reduce + (persistent! results))
;       (let [r2 (* r r)
;             more-results (for [q (range 2 r)
;                                :let [q2 (* q q)
;                                      a2 (+ r2 q2 (* r q))]
;                                :while (< a2 cap-half-square)
;                                :let [a (Math/sqrt a2)]
;                                :when (int? a)
;                                p (range 1 q)
;                                :let [p2 (* p p)
;                                      b2 (+ q2 p2 (* q p))
;                                      c2 (+ r2 p2 (* r p))
;                                      b (Math/sqrt b2)
;                                      c (Math/sqrt c2)
;                                      perim (+ a b c)]
;                                :when (and (<= perim cap) (int? b) (int? c))]
;                            (+ r q p))]
;         (recur (inc r) (reduce conj! results more-results))))))
; 
