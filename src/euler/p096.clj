(ns euler.p096)

(defn str-2-num-vec [s]
  (vec (map #(read-string (str %)) s)))

(def puzzles
  (map #(vec (map (fn [line] (str-2-num-vec line)) (drop 1 %)))
    (partition 10
      (clojure.string/split
        (slurp (.getFile (clojure.java.io/resource "sudoku.txt")))
        #"\n"))))

(defn box-offsets [y0 x0]
  (for [y (range 3) x (range 3)] 
    [(+ y0 y) (+ x0 x)]))

(defn non-zero [values]
  (filter (partial not= 0) values))

(defn row [puzzle y]
  (non-zero (get puzzle y)))
(defn col [puzzle x]
  (non-zero (map #(get-in puzzle [% x]) (range 9))))
(defn box [puzzle [y x]]
  (let [x0 (* (quot x 3) 3)
        y0 (* (quot y 3) 3)]
    (non-zero (map #(get-in puzzle %) (box-offsets y0 x0)))))

(defn row-vals [puzzle y]
  (set (row puzzle y)))

(defn col-vals [puzzle x]
  (set (col puzzle x)))

(defn box-vals [puzzle pos]
  (set (box puzzle pos)))

(def all-vals (set (range 1 10)))

(defn fewest-possible-location [puzzle]
  (reduce
    (fn [[fewest pos choices] [y x]]
      (let [solutions (clojure.set/difference 
                        all-vals 
                        (row-vals puzzle y) 
                        (col-vals puzzle x) 
                        (box-vals puzzle [y x]))]
        (if (< (count solutions) fewest)
          [(count solutions) [y x] solutions]
          [fewest pos choices])))
    [10 nil nil]
    (for [y (range 9)
          x (range 9)
          :when (zero? (get-in puzzle [y x]))]
      [y x])))

(defn no-duplicate? [nums] (= (count nums) (count (distinct nums))))

(defn valid? [puzzle]
  (when puzzle
    (and
      (every? no-duplicate? (map (partial row puzzle) (range 9)))
      (every? no-duplicate? (map (partial col puzzle) (range 9)))
      (every? no-duplicate?
        (map (partial box puzzle)
          (for [y (range 0 9 3)
                x (range 0 9 3)]
            [y x]))))))

(defn filled? [puzzle]
  (empty? (filter zero? (flatten puzzle))))

(defn solve-simple [puzzle]
  (let [reduced-puzzle (reduce
                         (fn [puzzle [y x]]
                           (let [solutions (clojure.set/difference 
                                             all-vals 
                                             (row-vals puzzle y) 
                                             (col-vals puzzle x) 
                                             (box-vals puzzle [y x]))]
                             (if (not= (count solutions) 1)
                               puzzle
                               (assoc-in puzzle [y x] (first solutions)))))
                         puzzle
                         (for [y (range 9) 
                               x (range 9)
                               :when (zero? (get-in puzzle [y x]))] 
                           [y x]))]
     (if (not= reduced-puzzle puzzle)
       (recur reduced-puzzle)
       puzzle)))

(defn solve-guess [puzzle]
  (let [[_ pos choices] (fewest-possible-location puzzle)]
    (loop [choices choices]
      (let [choice (first choices)
            puzzle (assoc-in puzzle pos choice)]
        (if (nil? choice)
          nil
          (do
            (let [reduced (solve-simple puzzle)
                  valid? (valid? reduced)
                  filled? (filled? reduced)]
              (cond
                (and valid? filled?) reduced
                valid? (let [solved (solve-guess reduced)]
                         (if solved 
                           solved 
                           (recur (rest choices))))
                :else (recur (rest choices))))))))))

(reduce +
  (map
    (fn [p]
      (clojure.pprint/pprint p)
      (let [[a b c] (take 3 (first p))]
        (+ (* a 100) (* b 10) c)))
    (map solve-guess puzzles)))
