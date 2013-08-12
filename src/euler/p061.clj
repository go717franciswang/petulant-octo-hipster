(ns euler.p061)

(defn shapes [f] (set (take-while #(< % 10000) (drop-while #(< % 1000) (map f (rest (range)))))))

(def triangles (shapes #(* % (inc %) 1/2)))
(def squares (shapes #(* % %)))
(def pentagons (shapes #(* % (dec (* 3 %)) 1/2)))
(def hexagons (shapes #(* % (dec (* 2 %)))))
(def heptagons (shapes #(* % (- (* 5 %) 3) 1/2)))
(def octagons (shapes #(* % (- (* 3 %) 2))))

(defn identify [n]
  (filter identity
    (map 
      (fn [s v]
        (when (contains? s n) v))
      [triangles squares pentagons hexagons heptagons]
      [:triangle :square :pentagon :hexagon :heptagon])))

(def find-from #{:triangle :square :pentagon :hexagon :heptagon})

(def init (for [x octagons] [[x] find-from]))

(defn concat-nums [a b]
  (+ (* a 100) b))

(defn expand-num [n]
  (let [base (mod n 100)]
    (map (partial concat-nums base) (range 10 100))))

((comp (partial reduce +) first first)
  (loop [results init
         iterations 1]
    (if (= iterations 6)
      (filter
        (fn [[num-vec _]]
          (= (quot (first num-vec) 100) (mod (last num-vec) 100)))
        results)
      (recur 
        (for [[num-vec find-from] results
              :let [last-num (last num-vec)]
              expanded-num (expand-num last-num)
              shape (identify expanded-num)
              :when (contains? find-from shape)]
          [(conj num-vec expanded-num) (disj find-from shape)])
        (inc iterations)))))
