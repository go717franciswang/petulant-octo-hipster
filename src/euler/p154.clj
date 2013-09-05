(ns euler.p154)

; http://en.wikipedia.org/wiki/Trinomial_expansion
; coefficient = n!/(i!j!k!)
; let i <= j <= k for now, count of total arrangement = * 6

(def n!-trailing-0s 
  (memoize
    (fn [n]
      (loop [d 5
             r 0]
        (let [q (quot n d)]
          (if (> q 0)
            (recur (* d 5) (int (+ r q)))
            r))))))

(def div 2E5)

(def numerator-0s (n!-trailing-0s div))

(def min-k
  (loop [k (dec div)]
    (if (> (- numerator-0s 12) (n!-trailing-0s (* 3 k)))
      (inc k)
      (recur (dec k)))))

(def target-denom-0s (- numerator-0s 12))

(def m {3 1 2 3 1 6})

(- (* (+ div 2) (inc div) 0.5)
  (reduce +
    (for [k (range div (dec min-k) -1)
          :let [denom-0s (n!-trailing-0s k)
                max-j (min (- div k) k)
                _ (println k)]
          j (range max-j 0 -1)
          :while (<= target-denom-0s (+ denom-0s (n!-trailing-0s (* 2 j))))
          :let [denom-0s (+ denom-0s (n!-trailing-0s j))
                max-i (min (- div k j) j)]
          i (range max-i -1 -1)
          :while (<= target-denom-0s (+ denom-0s (n!-trailing-0s i)))]
      (m (count (set [k j i]))))))

