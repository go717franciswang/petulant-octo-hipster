(ns euler.p088)

; paper on this subject "When the sum equals the product"
; http://www-users.mat.umk.pl/~anow/ps-dvi/si-krl-a.pdf
; Thm 4: x1 * .. * xn-1 <= n - 1
; Thm 6: x1 + .. + xn <= 2n to bound search space

(def max-size 12000)

(def factors 
  (loop [regens (map #(vector [%] %) (range 2 (inc (* max-size))))
         results []]
    (let [more-results (for [[regen product] regens
                             :let [cap (last regen)]
                             next-factor (range 2 (inc cap))
                             :let [next-product (* next-factor product)]
                             :while (<= next-product (* 2 max-size))]
                         [(conj regen next-factor) next-product])
          new-regens (filter
                       (fn [[regen product]]
                         (< (* product 2) (* max-size 2)))
                       more-results)
          new-results (concat results more-results)]
      (if (empty? new-regens)
        new-results
        (recur new-regens new-results)))))

(def inf (/ 1 0.0))

(def min-product-sums
  (reduce 
    (fn [m [regen product]]
      (let [sum (reduce + regen)
            size (+ (count regen) (- product sum))
            best-sum (get m size inf)]
        (if (< product best-sum)
          (assoc m size product)
          m)))
    {}
    factors))
                           
(reduce +
  (distinct
    (map second
      (filter #(> max-size (first %)) min-product-sums))))

