(ns euler.p235)

(defn u [k r]
  (* (- 300 k) (Math/pow r (dec k))))

(defn s [r]
  (loop [k 1
         s 0.0]
    (if (> k 5000)
      s
      (recur (inc k) (+ s (u k r))))))

(def target -2E11)

(loop [a 1.002
       b 1.005]
  (if (< (- b a) 1E-12)
    a
    (let [i (/ (- b a) 3)
          c (+ a i)
          d (+ c i)
          cs (double (s c))
          ds (double (s d))]
      (if (< (Math/abs (- cs target)) (Math/abs (- ds target)))
        (recur a d)
        (recur c b)))))
