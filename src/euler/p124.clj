(ns euler.p124
  (:require euler.helper))

(def cap 1E5)

(def primes (euler.helper/primes cap))

; find highest number that is the product of some distinct primes
(loop [primes primes
       results []]
  (if (empty? primes)
    (last (sort results))
    (let [more-results (for [n results
                             :let [new-n (* n (first primes))]
                             :while (<= new-n cap)]
                         new-n)
          results (doall (concat [(first primes)] results more-results))]
      (recur (rest primes) results))))

