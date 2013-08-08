(ns euler.p027)

(defn f [a b]
  (fn [n]
    (+ (* n n) (* a n) b)))

(def primes 
  (loop [p [2]
         i 3]
    (if (> i 1601)
      (set p)
      (let [sqrt (Math/pow i 0.5)]
        (if (some #(zero? (mod i %)) (take-while #(<= % sqrt) (rest p)))
          (recur p (+ i 2))
          (recur (conj p i) (+ i 2)))))))

(let [[_ a b] (reduce
                (fn [[max-n max-a max-b] [a b]]
                  (let [f (f a b)]
                    (if (and 
                          (contains? primes (f max-n))
                          (every? #(contains? primes (f %)) (range 0 max-n)))
                      [(last (take-while #(contains? primes (f %)) (drop max-n (range)))) a b]
                      [max-n max-a max-b])))
                [0 0 0]
                (for [a (range -999 1000)
                      b (range -999 1000)]
                  [a b]))]
  (* a b))
