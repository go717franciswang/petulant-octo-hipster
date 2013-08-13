(ns euler.p073)

; should be solved with farey's series

(defn fractions-btw-ab-with-denom [a b d]
  (let [floor (inc (int (* d a)))
        ceiling (let [p (* d b)] (if (integer? p) p (inc (int p))))]
  (for [x (range floor ceiling)]
    (/ x d))))

(count (distinct (flatten (map (partial fractions-btw-ab-with-denom 1/3 1/2) (range 1 12001)))))
