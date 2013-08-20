(ns euler.p110
  (:require euler.helper))

(def primes (euler.helper/primes 10000))

(defn num-solutions [prime-map]
  (/
    (inc (reduce *
      (map 
        (fn [[_ factors-count]]
          (inc (* 2 factors-count)))
        prime-map))) 2))

(defn number [prime-factors]
  (reduce * (bigint 1) prime-factors))

; we know the final answer will be consisted of no more than the first 15 prime numbers
; b/c they form a number that have at least 4M solutions
(def prime-count 15)
(def num-cap (reduce * (take prime-count primes)))

(def twos-needed-to-reach-cap (int (/ (Math/log num-cap) (Math/log 2))))

(defn pow [n i]
  (reduce * (bigint 1) (repeat i n)))

(def init-results 
  (for [i (range 1 (inc twos-needed-to-reach-cap))]
    [(pow 2 i) (sorted-map 2 i)]))

(def prime-factor-candidates
  (loop [primes (rest (take prime-count primes))
         results init-results]
    (println (first primes) (count results))
    (if (empty? primes)
      results
      (let [p (first primes)
            new-results (for [[n prime-map] results
                              :let [earlier-prime-cap (last (last prime-map))
                                    i-cap (min earlier-prime-cap
                                               (int (/ (Math/log (/ num-cap n)) (Math/log p))))]
                              i (range 0 (inc i-cap))]
                            [(* n (pow p i)) (conj prime-map [p i])])]
        (recur (rest primes) new-results)))))

(apply min
  (map first
    (filter 
      (fn [[_ prime-map]]
        (> (num-solutions prime-map) 4E6))
      prime-factor-candidates)))
