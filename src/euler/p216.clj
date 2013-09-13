(ns euler.p216)

;; bruteforce
; (loop [n 2N
;        c 0]
;   (if (> n 5E7)
;     c
;     (let [t (dec (* 2 n n))]
;       (if (.isProbablePrime (biginteger t) 20)
;         (recur (inc n) (inc c))
;         (recur (inc n) c)))))

(def size 5E7)

(def sieve (boolean-array (inc size) true))
(def ts 
  (let [ts (long-array (inc size))]
    (loop [n 2]
      (when (<= n size)
        (aset ^longs ts n (dec (* 2 n n)))
        (recur (inc n))))
    ts))

; http://109.90.219.190/devalco/quadr_Sieb_2x%5E2-1.php
(time
  (loop [n 2
         c 0]
    (if 
      (> n size) c
      (let [prime? (aget ^booleans sieve n)
            t (aget ^longs ts n)]
        (when (> t 1)
          (loop [n0 (- t n)]
            (when (<= n0 size)
              (aset ^booleans sieve n0 false)
              (while (zero? (mod (aget ^longs ts n0) t))
                (aset ^longs ts n0 (long (/ (aget ^longs ts n0) t))))
              (recur (+ n0 t))))
          (loop [n0 (+ t n)]
            (when (<= n0 size)
              (aset ^booleans sieve n0 false)
              (while (zero? (mod (aget ^longs ts n0) t))
                (aset ^longs ts n0 (long (/ (aget ^longs ts n0) t))))
              (recur (+ n0 t)))))
        (if prime?
          (recur (inc n) (inc c))
          (recur (inc n) c))))))


