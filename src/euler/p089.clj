(ns euler.p089)

(def romans
  (clojure.string/split
    (slurp (clojure.java.io/resource "roman.txt"))
    #"\n"))

(def roman-map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000})

(defn split [r]
  (reduce
    (fn [v c]
      (let [last-c (first (last v))]
        (if (= last-c c)
          (update-in v [(dec (count v))] conj c)
          (assoc v (count v) [c]))))
    []
    (seq r)))

(defn roman-2-int [r]
  (loop [s (split r)
         a 0]
    (if (empty? s)
      a
      (let [value (reduce + (map roman-map (first s)))
            next-val (if (> (count s) 1) (roman-map (first (second s))) 0)]
        (if (< value next-val)
          (recur (rest s) (- a value))
          (recur (rest s) (+ a value)))))))

(defn to-roman [n]
  "ported from 4clojure p104 solution"
  (apply str (let [a (quot n 1000)
                   b (quot (- n (* a 1000)) 100)
                   c (quot (- n (* a 1000) (* b 100)) 10)
                   d (quot (- n (* a 1000) (* b 100) (* c 10)) 1)]
               [(case a
                  0 nil
                  1 "M"
                  2 "MM"
                  3 "MMM"
                  4 "MMMM")
                (case b
                  0 nil
                  1 "C"
                  2 "CC"
                  3 "CCC"
                  4 "CM"
                  5 "D"
                  6 "DC"
                  7 "DCC"
                  8 "DCCC"
                  9 "CM")
                (case c
                  0 nil
                  1 "X"
                  2 "XX"
                  3 "XXX"
                  4 "XL"
                  5 "L"
                  6 "LX"
                  7 "LXX"
                  8 "LXXX"
                  9 "XC")
                (case d 0 nil
                  1 "I"
                  2 "II"
                  3 "III"
                  4 "IV"
                  5 "V"
                  6 "VI"
                  7 "VII"
                  8 "VIII"
                  9 "IX")])))

(reduce +
  (for [r romans]
    (let [i (roman-2-int r)
          reduced (to-roman i)
          saved (- (count r) (count reduced))]
      saved)))
