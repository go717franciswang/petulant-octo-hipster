(ns euler.p116)

(def colored-start 
  (memoize
    (fn [length min-length]
      (if (< length min-length)
        0
        (let [colored-length min-length]
          (reduce +
            (for [black-length (range (inc (- length colored-length)))
                  :let [left (- length colored-length black-length)]]
              (if (zero? left)
                1
                (colored-start left min-length)))))))))

(defn any-start [length min-length]
  (reduce +
    (for [black-length (range (inc (- length min-length)))
          :let [left (- length black-length)]]
      (colored-start left min-length))))

(defn reds [length] (any-start length 2))
(defn greens [length] (any-start length 3))
(defn blues [length] (any-start length 4))

(defn all-ways [length]
  (+ (reds length)
     (greens length)
     (blues length)))

(all-ways 50)
