(ns euler.p301)

(def cap (Math/pow 2 30))

(time
  (loop [n 1
         c 0N]
    (if (> n cap)
      c
      (recur (inc n) (if (zero? (bit-xor n (+ n n) (+ n n n))) (inc c) c)))))
