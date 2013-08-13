(ns euler.p066)

(defn expand [[q [x a b]]]
  ;a * sqrt(x) + b => q + 1 / (new-a * sqrt(x) + new-b)
  (let [nq (int (+ (* a (Math/pow x 0.5)) b))
        t (- b nq)
        na a
        nb (- t)
        nc (- (* a a x) (* t t))]
    [nq [x (/ na nc) (/ nb nc)]]))

(defn qs [x]
  (map first (rest (iterate expand [0 [x 1 0]]))))

(defn convergents [x]
  (let [qs (qs x)]
    (for [x (rest (range))]
      (let [reverse-qs (reverse (take x qs))]
        (loop [factors (rest reverse-qs)
               result (first reverse-qs)]
          (if (empty? factors)
            result
            (recur (rest factors) (+ (first factors) (/ 1 result)))))))))

(defn square? [x]
  (let [a (Math/pow x 0.5)]
    (= a (float (int a)))))

(def check-ints (filter (comp not square?) (range 1001)))

(defn minimal-solution [d]
  (if (square? d)
    0
    ((comp first first)
      (filter
        (fn [[x y]] (= 1 (- (* x x) (* d y y))))
        (map 
          (fn [convergent]
            (if (integer? convergent)
              [convergent 1]
              (let [x (numerator convergent)
                    y (denominator convergent)]
                [x y])))
          (convergents d))))))

((comp first last)
  (sort-by second
    (map 
      (fn [i]
        (let [a (minimal-solution i)]
          (println i a)
          [i a]))
      check-ints)))
