(ns euler.p166)

; [a b c d
;  e f g h
;  i j k l
;  m n o p]

; bruteforce
(defn combos [sum]
  (let [a
    (reduce + 0
      (for [a (range (inc (min 9 sum)))
            b (range (max 0 (- sum a 9 9)) (inc (min 9 (- sum a))))
            c (range (max 0 (- sum a b 9)) (inc (min 9 (- sum a b))))
            :let [d (- sum a b c)]
            e (range (max 0 (- sum a 9 9)) (inc (min 9 (- sum a))))
            f (range (max 0 (- sum b 9 9) (- sum e 9 9) (- sum a 9 9)) 
                     (inc (min 9 (- sum b) (- sum e) (- sum a))))
            g (range (max 0 (- sum c 9 9) (- sum e f 9) (- sum d 9 9)) 
                     (inc (min 9 (- sum c) (- sum e f) (- sum d))))
            :let [h (- sum e f g)]
            i (range (max 0 (- sum a e 9)) (inc (min 9 (- sum a e))))
            j (range (max 0 (- sum b f 9) (- sum i 9 9) (- sum d g 9)) 
                     (inc (min 9 (- sum b f) (- sum i) (- sum d g))))
            k (range (max 0 (- sum c g 9) (- sum i j 9) (- sum a f 9))
                     (inc (min 9 (- sum c g) (- sum i j) (- sum a f))))
            :let [l (- sum i j k)
                  m (- sum a e i)
                  n (- sum b f j)
                  o (- sum c g k)
                  p (- sum d h l)]
            :when (and (= (+ m n o p) sum)
                       (= (+ a f k p) sum)
                       (= (+ d g j m) sum))]
        1))]
    (println sum a)))

; got a weird error and had to manually add the numbers together
(reduce + (combos 18)
  (pmap combos (range 18)))
