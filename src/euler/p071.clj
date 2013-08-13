(ns euler.p071)

(def size 1000000)

(def around 3/7)
(def denom (denominator around))

(reduce
  (fn [[md mx] [d x]]
    (if (< d md)
      [d x]
      [md mx]))
  (for [x (rest (range size))
        :let [y (/ (int (* x around)) x)]
        :when (and
                (not (zero? (mod x denom)))
                (not (integer? y)))]
    (let [y (/ (int (* x around)) x)]
      [(- around y) (numerator y)])))
