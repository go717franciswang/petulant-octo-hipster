(ns euler.p207)

; 4^t = 2^t + k
; => a^2 - a - k = 0 s.t a = 2^t => t = ln(a)/ln(2)
; => a = (1+sqrt(1+4k)) / 2
; => k = ((2a - 1)^2 - 1) / 4

(defn get-k [a]
  (/ (dec (Math/pow (dec (* 2 a)) 2)) 4))

(def log2 (Math/log 2))

; iterating over a is much more efficient than iterating over k
(loop [a 3
       partitions 1
       perfects 1]
  (if (< (/ perfects partitions) 1/12345)
    (bigint (get-k (dec a)))
    (let [k (get-k a)
          partitions? (== k (bigint k))
          perfect? (let [t (/ (Math/log a) log2)]
                     (== t (bigint t)))]
      (recur (inc a)
             (if partitions? (inc partitions) partitions)
             (if perfect? (inc perfects) perfects)))))
