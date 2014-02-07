(ns euler.p220)

(defn transform [^String D0]
  (loop [i 0
         D1 ""]
    (let [c (if (< i (.length D0)) (.charAt D0 i) nil)]
      (condp = c
        nil D1
        \a (recur (inc i) (str D1 "aRbFR"))
        \b (recur (inc i) (str D1 "LFaLb"))
        (recur (inc i) (str D1 c))))))

(defn locate [^String D]
  (loop [i 0
         x 0
         y 0
         dx 1
         dy 0]
    (let [c (if (< i (.length D)) (.charAt D i) nil)]
      (condp = c
        nil [x y]
        \R (recur (inc i) x y dy (- dx))
        \L (recur (inc i) x y (- dy) dx)
        \F (recur (inc i) (+ x dx) (+ y dy) dx dy)
        (recur (inc i) x y dx dy)))))

; brute-force to observe some pattern
(loop [i 0
       D "Fa"]
  (when (< i 11)
    (let [D (transform D)]
      (println i (.length D) (locate D))
      (recur (inc i) D))))

