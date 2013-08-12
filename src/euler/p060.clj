(ns euler.p060)

(def primes 
  (loop [p [2]
         i 3]
    (if (> i 10000)
      p
      (let [sqrt (Math/pow i 0.5)]
        (if (some #(zero? (mod i %)) (take-while #(<= % sqrt) (rest p)))
          (recur p (+ i 2))
          (recur (conj p i) (+ i 2)))))))

(defn prime? [n]
  (loop [checks (range 2 (+ 2 (int (Math/pow n 0.5))))]
    (cond
      (empty? checks) true
      (zero? (mod n (first checks))) false
      :else (recur (rest checks)))))

(defn concat-nums [a b]
  (read-string (str a b)))

(defn related? [a b]
  (and
    (prime? (concat-nums a b))
    (prime? (concat-nums b a))))

(for [x1 primes
      x2 (drop (inc (.indexOf primes x1)) primes)
      :when (related? x1 x2)
      x3 (drop (inc (.indexOf primes x2)) primes)
      :when (every? (partial related? x3) [x1 x2])
      x4 (drop (inc (.indexOf primes x3)) primes)
      :when (every? (partial related? x4) [x1 x2 x3])
      x5 (drop (inc (.indexOf primes x4)) primes)
      :when (every? (partial related? x5) [x1 x2 x3 x4])]
  [x1 x2 x3 x4 x5])
              

