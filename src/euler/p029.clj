(ns euler.p029)

(count
  (reduce conj #{}
    (for [a (range 2 101)
          b (range 2 101)]
      (Math/pow a b))))
