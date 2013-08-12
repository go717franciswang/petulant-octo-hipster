(ns euler.p064)

(defn expand [[q [x a b]]]
  ;a * sqrt(x) + b => q + 1 / (new-a * sqrt(x) + new-b)
  (let [nq (int (+ (* a (Math/pow x 0.5)) b))
        t (- b nq)
        na a
        nb (- t)
        nc (- (* a a x) (* t t))]
    [nq [x (/ na nc) (/ nb nc)]]))

(defn sqrt-to-int? [x]
  (let [a (Math/pow x 0.5)]
    (= a (float (int a)))))

(defn period [x]
  (if (sqrt-to-int? x)
    0
    (loop [expansions [[0 [x 1 0]]]]
      (let [expansion (expand (last expansions))
            i (.indexOf expansions expansion)]
        (if (not= i -1)
          (- (count expansions) i)
          (recur (conj expansions expansion)))))))

(count (filter odd? (map period (range 2 10001))))
