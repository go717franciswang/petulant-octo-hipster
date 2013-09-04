(ns euler.p152
  (:require [euler.helper :as h]
            [clojure.math.combinatorics :as combo]))

(def cap 45)

(def primes (h/primes cap))

(def num-set 
  (memoize
    (fn [p]
      (loop [n p
             result []]
        (if (> n cap)
          (map #(set (second %)) (filter #(not= 0 (mod (denominator (first %)) p)) result))
          (if (some #(zero? (mod n %)) (drop-while #(< % 14) primes))
            (recur (+ n p) result)
            (let [inv (/ 1 n n)]
              (recur 
                (+ n p)
                (concat result
                        (map 
                          (fn [[sum nums]]
                            [(+ sum inv) (conj nums n)])
                          result)
                        [[inv [n]]])))))))))

(defn not-divisible-by [nums qs]
  (every? (fn [q]
            (every? #(not= 0 (mod % q)) nums))
          qs))

(defn join-set [as bs non-qs]
  (for [a as
        b bs
        :when (not-divisible-by (clojure.set/difference b a) non-qs)]
    (clojure.set/union a b)))

(def init-result
  (loop [n 2
         result []]
    (if (> n cap)
      (conj (set (map (fn [nums]
                  (reduce + (map #(/ 1 % %) nums)))
                result)) 0)
      (recur (* n 2) (concat result (map #(conj % n) result) [#{n}])))))

(time
(doseq [x (rest primes)]
  (println (count (num-set x)))))


; (def candidates
;   (let [ps [3 5 7 13]
;         num-sets {3 (num-set 3)
;                   5 (num-set 5)
;                   7 (num-set 7)
;                   13 (num-set 13)}]
;     (flatten
;       (for [size (range 1 (inc (count ps)))
;             primes (combo/combinations ps size)
;             :let [_ (println primes)]]
;         (loop [result (num-sets (first primes))
;                non-qs [(first primes)]
;                primes (rest primes)]
;           (if (empty? primes)
;             result
;             (recur (join-set result (num-sets (first primes)) non-qs) 
;                    (conj non-qs (first primes)) 
;                    (rest primes))))))))
; 
; (println (count candidates))
; 
; (defn valid? [candidate]
;   (let [a (reduce + (map #(/ 1 % %) candidate))]
;     (get init-result (- 1/2 a))))
; 
; (count (set (filter valid? candidates)))
; 
