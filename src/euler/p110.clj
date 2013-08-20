(ns euler.p110
  (:require euler.helper))

(def size 1000)

(def primes (euler.helper/primes 10000))

(defn num-solutions [prime-factors]
  (/
    (inc (reduce *
      (map 
        (fn [[_ factors]]
          (inc (* 2 (count factors))))
        (group-by identity prime-factors)))) 2))

(defn number [prime-factors]
  (reduce * (bigint 1) prime-factors))

(def num-cap (reduce * (take 15 primes)))

(loop [

(loop [prime-factors [2]
       original-n 2
       original-solutions 2]
  ;(println prime-factors)
  (let [max-factor (reduce max prime-factors)
        try-upto (+ 2 (.indexOf primes max-factor))
        tries (take try-upto primes)
        _ (println tries)
        new-factor (first (last
                     (sort-by second
                       (map
                         (fn [prime]
                           (let [new-factors (conj prime-factors prime)
                                 solutions (num-solutions new-factors)
                                 n (number new-factors)
                                 r (/ (- solutions original-solutions) (float (- n original-n)))]
                           [prime r]))
                         tries))))
        new-factors (conj prime-factors new-factor)]
    (let [solutions (num-solutions new-factors)]
      (if (> solutions size)
        (* original-n new-factor)
        (recur new-factors (* original-n new-factor) solutions)))))
