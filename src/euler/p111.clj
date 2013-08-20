(ns euler.p111
  (:require [clojure.math.combinatorics :as combo]))

(def digit-size 10)

(defn pow-10 [n]
  (reduce * (repeat n 10)))

(defn permuate-n-digits-without-x [n x]
  (let [valid-nums (concat (range x) (range (inc x) 10))]
    (loop [results (map vector valid-nums)
           n (dec n)]
      (if (zero? n)
        results
        (recur
          (for [r results
                v valid-nums]
            (conj r v))
          (dec n))))))

(defn construct-num [position foreign-digits native-num]
  (let [pos-2-num (apply hash-map (interleave position foreign-digits))
        default native-num
        digits (map #(get pos-2-num % default) (range digit-size))]
    (when (not= (first digits) 0)
      (read-string (apply str digits)))))

(defn int-runs [d run-length]
  (let [foreign-num-count (- digit-size run-length)
        positions (combo/combinations (range digit-size) foreign-num-count)]
    (for [position positions
          foreign-digits (permuate-n-digits-without-x foreign-num-count d)
          :let [new-num (construct-num position foreign-digits d)]
          :when new-num]
      new-num)))

(defn prime-runs [d]
  (loop [run-length (dec digit-size)]
    (let [primes (filter
                   (fn [n]
                     (.isProbablePrime (biginteger n) 15))
                   (int-runs d run-length))]
      (if (empty? primes)
        (recur (dec run-length))
        primes))))

(defn S [d]
  (reduce + (prime-runs d)))

(reduce + (map S (range 10)))
