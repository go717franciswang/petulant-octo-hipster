(ns euler.p052)

(defn same-digits? [a b]
  (= (sort (str a))
     (sort (str b))))

(first
  (filter
    (fn [a]
      (and
        (same-digits? a (* 2 a))
        (same-digits? a (* 3 a))
        (same-digits? a (* 4 a))
        (same-digits? a (* 5 a))
        (same-digits? a (* 6 a))))
  (rest (range))))
