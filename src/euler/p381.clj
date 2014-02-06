(ns euler.p381
  (:require [euler.helper :as h]))

(defn factorial [n p]
  (reduce 
    (fn [a b]
      (mod (* a b) p))
    1N
    (range 1 (inc n))))

(defn S-bruteforce [p]
  (mod (reduce + (map #(factorial % p) (range (- p 5) p))) p))

(def primes (drop 2 (h/primes3 (long 1E8))))

; observe pattern that (p-1)! mod p = p-1, but why?!
; (doseq [p primes]
;   (println p (factorial (- p 4) p)))

(defn mod-inv [n m]
  (.modPow (biginteger n) (biginteger -1) (biginteger m)))

(defn S [p]
  (let [f1 (dec p)
        f2 (mod (* (mod (* f1) p) (mod-inv (- p 1) p)) p)
        f3 (mod (* (mod (* f2) p) (mod-inv (- p 2) p)) p)
        f4 (mod (* (mod (* f3) p) (mod-inv (- p 3) p)) p)
        f5 (mod (* (mod (* f4) p) (mod-inv (- p 4) p)) p)]
    (mod (+ f1 f2 f3 f4 f5) p)))

(reduce + (map S primes))
