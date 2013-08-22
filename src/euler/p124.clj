(ns euler.p124
  (:require euler.helper))

(def cap 1E5)

(def idx 1E4)

(def primes (euler.helper/primes cap));(int (* 0.2 cap))))

(defn prime-factors [n]
  (loop [n n
         ps primes
         result #{1}]
    (if (= n 1)
      result
      (let [p (first ps)]
      (if (zero? (mod n p))
        (recur (/ n p) ps (conj result p))
        (recur n (rest ps) result))))))

(defn rad [n]
  (reduce * (prime-factors n)))

(nth (sort-by rad (range 1 (inc cap))) (dec idx))

; (def sorted-radic-factors
;   (loop [primes primes
;          results []]
;     (if (empty? primes)
;       (map second (sort-by first results))
;       (let [p (first primes)
;             more-results (for [[product nums] results
;                                :let [new-product (* product p)
;                                      new-nums (conj nums p)]
;                                :while (<= new-product cap)]
;                            [new-product new-nums])
;             results (doall (concat [[p [p]]] results more-results))]
;         (recur (rest primes) results)))))
; 
; (defn nums [factors]
;   (loop [factors factors
;          nums []]
;     (if (empty? factors)
;       nums
;       (recur
;         (rest factors)
;         (if (empty? nums)
;           (for [x (rest (range))
;                 :let [n (int (Math/pow (first factors) x))]
;                 :while (<= n cap)]
;             n)
;           (for [n nums
;                 x (rest (range))
;                 :let [new-n (* n (int (Math/pow (first factors) x)))]
;                 :while (<= new-n cap)]
;             new-n))))))

; (loop [factors sorted-radic-factors
;        nums-count 1]
;   (let [nums-with-same-radic (nums (first factors))
;         new-nums-count (+ nums-count (count nums-with-same-radic))]
;     (println (first factors) (reduce * (first factors)) new-nums-count nums-with-same-radic)
;     (if (< new-nums-count idx)
;       (recur (rest factors) new-nums-count)
;       (first (drop (- idx nums-count 1) (vec (sort nums-with-same-radic)))))))
; 
