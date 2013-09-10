(ns euler.p197)

(defn f [x]
  (/ (Math/floor (Math/pow 2 (- 30.403243784 (* x x)))) 1E9))

(def u_n
  (loop [m {-1 0}
         r [-1]
         n 1]
    (let [v (f (get r (dec n)))]
      (if (contains? m v)
        (let [n0 (get m v)
              cycle-l (- n n0)]
          (println n n0 cycle-l (+ n0 (mod (- 1E12 n0) cycle-l)))
          (get r (int (+ n0 (mod (- 1E12 n0) cycle-l)))))
        (recur (conj m [v n]) (conj r v) (inc n))))))

(+ u_n (f u_n))
