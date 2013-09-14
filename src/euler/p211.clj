(ns euler.p211
  (:require [euler.helper :as h]))

; bruteforce
(def size 64E6)

#_(time
(def primes (h/primes3 size)))

(time
(def square-sums (long-array size 1)))

(time
(loop [x 2]
  (when (zero? (mod x 1E3)) (println x))
  (when (< x size)
    (let [xx (* x x)]
      (loop [y x]
        (when (< y size)
          (aset ^longs square-sums y (+ (aget ^longs square-sums y) xx))
          (recur (+ y x))))
      (recur (inc x))))))

(defn square? [z]
  (let [a (Math/sqrt z)]
    (== a (bigint a))))

#_(defn square? [z]
  (loop [z z
         ps primes]
    (let [p (first ps)
          pp (* p p)]
      (cond
        (== z 1) true
        (> pp z) false
        (zero? (mod z p)) (if (zero? (mod z pp))
                            (recur (/ z pp) ps)
                            false)
        :else (recur z (rest ps))))))

#_(defn square? [z]
  (loop [l (double 1)
         h z]
    (let [m (quot (+ h l) 2)
          mm (* m m)]
      (cond
        (> l h) false
        (== z mm) true
        (< z mm) (recur l (dec m))
        :else (recur (inc m) h)))))

(time
(loop [n 1
       s 0]
  (if (< n size)
    (if (square? (aget ^longs square-sums n))
      (do (println n) (recur (inc n) (+ s n)))
      (recur (inc n) s))
    s)))
