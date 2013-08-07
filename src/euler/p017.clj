(def m
  (let [init {1 "one"
              2 "two"
              3 "three"
              4 "four"
              5 "five"
              6 "six"
              7 "seven"
              8 "eight"
              9 "nine"
              10 "ten"
              11 "eleven"
              12 "twelve"
              13 "thirteen"
              14 "fourteen"
              15 "fifteen"
              16 "sixteen"
              17 "seventeen"
              18 "eighteen"
              19 "nineteen"
              20 "twenty"
              30 "thirty"
              40 "forty"
              50 "fifty"
              60 "sixty"
              70 "seventy"
              80 "eighty"
              90 "ninety"
              1000 "one thousand"}]
    (reduce (fn [m v] (assoc m (* v 100) (str (get m v) " hundred"))) init (range 1 10))))

(defn stringify [n]
  (cond 
    (= n 1000) (get m n)
    (>= n 100) (str (get m (* (int (/ n 100)) 100)) 
                    (if-let [more (stringify (rem n 100))]
                      (str " and " more)))
    (>= n 20) (str (get m (* (int (/ n 10)) 10))
                   (if-let [more (stringify (rem n 10))]
                     (str " " more)))
    :else (get m n)))

(defn count-char [n]
  (count (clojure.string/replace (stringify n) #"\s" "")))

(reduce + (map count-char (range 1 1001)))
