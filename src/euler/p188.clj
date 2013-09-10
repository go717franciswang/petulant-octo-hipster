(ns euler.p188)

(def length 8)
(def modder (reduce * (repeat length 10N)))

(defn truncated-multiply [a b]
  (let [r (mod (* a b) modder)]
    (if (< r (/ modder 10))
      (+ modder r)
      r)))

(defn pow-cycle [n]
  (let [n (bigint n)]
    (loop [n1 n
           i 1
           m {}
           r [1]]
      (if (m n1)
        (let [i0 (int (get m n1))]
          (fn [pow]
            (cond
              (zero? pow) 1
              (contains? r pow) (get r pow)
              :else (let [l (count r)
                          cycle-l (- l i0)
                          offset (mod (- pow i0) cycle-l)]
                      (get r (+ i0 offset))))))
        (let [v (truncated-multiply n1 n)]
          (recur v (inc i) (conj m [n1 i]) (conj r n1)))))))

(let [pow-1777 (pow-cycle 1777)]
  (loop [i 1
         v 1]
    (if (> i 1885)
      v
      (recur (inc i) (int (pow-1777 v))))))
