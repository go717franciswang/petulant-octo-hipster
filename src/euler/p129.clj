(ns euler.p129)

(defn A [n]
  (loop [k 1
         r 0]
    (if (zero? (mod n 5))
      0
      (let [r (mod (inc (* r 10)) n)]
        (if (zero? r)
          k
          (recur (inc k) r))))))

(time
  (loop [n (int (inc 1E6))]
    (let [a (A n)]
      (if (> a 1E6)
        n
        (recur (+ 2 n))))))

