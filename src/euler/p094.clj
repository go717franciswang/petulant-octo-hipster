(ns euler.p094
  (:require euler.helper))

(def n-k-ratio (Math/pow 3 0.5))

(def size 200000)

(def coprimes
  (filter 
    (fn [[k n]]
      (= (euler.helper/gcd n k) 1))
    (map 
      (fn [n] [(Math/round (/ n n-k-ratio)) n])
      (range 2 size))))

(reduce +
  (map (fn [[a c]] (+ a a c))
    (filter (fn [[a c]] (and 
                          (= 1 (Math/abs (int (- a c))))
                          (< (+ a a c) 1E9)))
      (for [[k n] coprimes]
        (let [am (+ (* n n) (* k k))
              cm (* 2 (- (* n n) (* k k)))
              gcd (euler.helper/gcd am cm)
              a (/ am gcd)
              c (/ cm gcd)]
          (when (= 1 (Math/abs (- a c)))
            (println n k a c))
          [a c])))))

      
