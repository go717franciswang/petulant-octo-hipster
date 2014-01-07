(ns euler.p250
  (:require [euler.helper :as h]))

; get frequency of pow-mod from 1^1 to 250250^250250
(def cap 250250)

(def freq
  (frequencies (pmap #(h/pow-mod % % 250) (range 1 (inc cap)))))

(def base-vals
  (vec (sort (keys freq))))

(def last-i
  (dec (count base-vals)))

(def all-combo-after 
  "return number of combinations for base-i and after"
  (memoize 
    (fn [i] 
      (if (> i last-i)
        1
        (* (h/big-pow 2 (get freq (get base-vals i))) 
           (all-combo-after (inc i)))))))
(doseq [i (reverse (range (inc last-i)))]
  (all-combo-after i))

(defn combinations [i pow-mod]
  (if (> i last-i)
    (if (zero? pow-mod) 1 0)
    (let [base (get base-vals i)
          occurance (get freq base)]
      (loop [j 0
             combo 0N]
        (if (> j occurance)
          combo
          (let [new-pow-mod (mod (* pow-mod (h/pow-mod base j 250)) 250)
                coeff (h/C occurance j)
                future-combo (if (zero? new-pow-mod)
                               (* coeff (all-combo-after (inc i)))
                               (* coeff (combinations (inc i) new-pow-mod)))
                new-combo (bigint (+ combo future-combo))]
            (recur (inc j) new-combo)))))))

(println freq)
(println base-vals)
(println last-i)

(time
  (combinations 0 1))
