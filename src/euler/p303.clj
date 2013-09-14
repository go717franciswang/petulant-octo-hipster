(ns euler.p303)

(defn valid? [n]
  (let [d (mod n 10)]
    (if (> d 2)
      false
      (if (< n 10)
        true
        (recur (quot n 10))))))

(defn f' [n]
  (let [last-digit (mod n 10)
        one-digit-candidates (map bigint 
                                  (filter #(valid? (mod (* % n) 10)) (range 1 10)))]
    (loop [digit-span 10
           candidates one-digit-candidates]
      (let [found (filter #(and (> % 0) (valid? (* n %))) candidates)]
        (if (not (empty? found))
          (reduce min found)
          (recur (* digit-span 10)
                 (for [candidate candidates
                       digit (range 10)
                       :let [new-candidate (+ (* digit-span digit) candidate)]
                       :when (valid? (mod (* new-candidate n) (* 10 digit-span)))]
                   new-candidate)))))))

(defn f'' [n]
  (let [a (f' n)]
    (println n a)
    a))

(time
(reduce +
  (pmap f'' (range 1 10001))))
