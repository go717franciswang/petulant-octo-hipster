(ns euler.p074)

(def fact
  (memoize
    (fn [n]
      (reduce * (rest (range (inc n)))))))

(def digit-fact-sum 
  (memoize
    (fn [n]
      (loop [n n
             r 0]
        (if (zero? n)
          r
          (recur (quot n 10) (+ r (fact (mod n 10)))))))))

(def chain-sizes (atom {}))

(defn chain-size [x]
  (loop [n x
         chain [x]]
    (let [nn (digit-fact-sum n)
          tail-size (@chain-sizes nn nil)]
      (if tail-size
        (let [a (+ tail-size (count chain))]
          (swap! chain-sizes assoc x a)
          a)
        (let [i (.indexOf chain nn)]
          (if (not= i -1)
            (do
              (let [a (count chain)]
                (swap! chain-sizes assoc x a)
                a))
            (recur nn (conj chain nn))))))))

(time (count (filter (partial = 60) (map chain-size (range 1000000)))))
