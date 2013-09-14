(ns euler.p234
  (:require [euler.helper :as h]))

(def cap 999966663333)

(def cap-sqrt (Math/sqrt cap))

(def primes (h/primes3 (int (* 1.5 cap-sqrt))))

(time
(loop [primes primes
       sum 0N]
  (if (> (first primes) cap-sqrt)
    sum
    (let [[p1 p2] (take 2 primes)
          pp1 (* p1 p1)
          pp2 (* p2 p2)
          max-pp2 (min cap pp2)
          divisible-by-p1 (loop [n (+ (- pp1 (mod pp1 p1)) p1)
                                 s #{}]
                            (if (< n max-pp2)
                              (recur (+ n p1) (conj s n))
                              s))
          divisible-by-p2 (loop [n (+ (- pp1 (mod pp1 p2)) p2)
                                 s #{}]
                            (if (< n max-pp2)
                              (recur (+ n p2) (conj s n))
                              s))
          divisible-by-only-1 (clojure.set/difference
                                (clojure.set/union
                                  divisible-by-p1
                                  divisible-by-p2)
                                (clojure.set/intersection divisible-by-p1 divisible-by-p2))]
      (recur (rest primes) (bigint (reduce + sum divisible-by-only-1)))))))

