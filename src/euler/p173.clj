(ns euler.p173)

(def cap 1E6)

(defn laminae [start-size]
  (loop [l start-size
         c 0N]
    (if (> l cap)
      c
      (let [n (Math/floor (/ (- (Math/sqrt (+ (Math/pow (- l 4) 2) (* 16 cap))) l -4) 8))
            u (+ l (* 8 (dec n)))]
        (recur (+ 8 l) (+ c (Math/round (inc (/ (- u l) 8)))))))))

(+ (laminae 8) (laminae 12))
