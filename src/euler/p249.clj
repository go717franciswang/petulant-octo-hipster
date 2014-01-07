(ns euler.p249
  (:require [euler.helper :as h]))

(def primes-small (vec (h/primes3 5000)))

(def max-num (reduce + primes-small))

(def S (long-array (inc max-num) 0))
(aset ^longs S 0 1) ; empty set sum up to 0

(def modular (long (h/big-pow 10 16)))

(loop [i 0]
  (println i)
  (when (< i (count primes-small))
    (let [p (get primes-small i)
          add-to (loop [j 0
                        add-to (transient {})]
                   (cond
                     (>= j max-num) (persistent! add-to)
                     (zero? (aget ^longs S j)) (recur (inc j) add-to)
                     :else (recur (inc j) (assoc! add-to (+ j p) (aget ^longs S j)))))]
      (doseq [[i v] add-to]
        (aset ^longs S i (long (mod (+ (aget ^longs S i) v) modular)))))
    (recur (inc i))))

(def primes-big (h/primes3 max-num))

(reduce 
  (fn [s x] (mod (+ s x) modular))
  (for [p primes-big]
    (aget ^longs S p)))
