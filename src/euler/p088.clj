(ns euler.p088)

; paper on this subject "When the sum equals the product"
; http://www-users.mat.umk.pl/~anow/ps-dvi/si-krl-a.pdf
; Thm 4: x1 * .. * xn-1 <= n - 1
; Thm 6: x1 + .. + xn <= 2n to bound search space

(defn min-product-sum [size]
  (let [suffix (loop [candidates (map #(vector [%] %) (range 1 (inc size)))]
                 (if (= (dec size) (count (first (first candidates))))
                   candidates
                   (recur 
                     (for [[values product] candidates
                           :let [max-val (last values)]
                           next-val (range 1 (inc (min (quot size product) max-val)))]
                       [(conj values next-val) (* product next-val)]))))]
    (reduce min
      (filter identity
        (map
          (fn [[series product]]
            (let [sum (reduce + series)
                  error (if (= 1 product) 1 (mod sum (dec product)))]
              (when (zero? error)
                (+ sum (quot sum (dec product))))))
          suffix)))))

(reduce +
  (set 
    (map 
      (fn [s]
        (let [a (min-product-sum s)]
          (println [s a])))
      (range 2 12001))))


      
