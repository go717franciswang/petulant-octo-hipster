(ns euler.p035)

(def primes 
  (set 
    (loop [p [2]
           i 3]
      (if (> i 1000000)
        (set p)
        (let [sqrt (Math/pow i 0.5)]
          (if (some #(zero? (mod i %)) (take-while #(<= % sqrt) (rest p)))
            (recur p (+ i 2))
            (recur (conj p i) (+ i 2))))))))

(defn rotations [n]
  (let [s (str n)
        l (count s)]
    (loop [r []
           s s]
      (let [s2 (apply str (take l (drop 1 (cycle s))))
            n2 (read-string (clojure.string/replace s2 #"^0+" ""))]
        (if (= n n2)
          r
          (recur (conj r n2) s2))))))
    
(count
  (filter
    (fn [prime]
      (every? #(contains? primes %) (rotations prime)))
    primes))


