(ns euler.p154)

; http://en.wikipedia.org/wiki/Trinomial_expansion
; coefficient = n!/(i!j!k!)
; let i+j+k = 2E6 and i <= j <= k for now, count of total arrangement = * 6

(defn n!-factor-k [n k]
  (loop [d k
         r 0]
    (let [q (quot n d)]
      (if (> q 0)
        (recur (* d k) (int (+ r q)))
        r))))

(def div (int 2E5))

(def n!-5s (vec (map #(n!-factor-k % 5) (range 0 (inc div)))))
(def n!-2s (vec (map #(n!-factor-k % 2) (range 0 (inc div)))))

(def m {3 1 2 3 1 6})

(def target-denom-5s (- (n!-5s div) 12))
(def target-denom-2s (- (n!-2s div) 12))

(defn valid? [n5 n2]
  (and (<= n5 target-denom-5s)
       (<= n2 target-denom-2s)))
(println target-denom-2s target-denom-5s)

(reduce +
  (pmap
    (fn [i]
      (let [max-j (quot (- div i) 2)
            n5 (n!-5s i)
            n2 (n!-2s i)
            i=j (if (valid? (+ n5 n5 (n!-5s (- div i i))) (+ n2 n2 (n!-2s (- div i i)))) 3 0)
            j=k (if (and (zero? (mod (- div i) 2))
                        (valid? (+ n5 (* 2 (n!-5s max-j))) (+ n2 (* 2 (n!-2s max-j))))) 3 0)]
        (apply +
          i=j j=k
          (for [j (range (inc i) (if (zero? (mod (- div i) 2)) max-j (inc max-j)))
                :let [k (- div i j)]
                :when (valid? (+ n5 (n!-5s j) (n!-5s k)) (+ n2 (n!-2s j) (n!-2s k)))]
            6))))
    (range (inc (quot div 3)))))

