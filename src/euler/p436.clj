(ns euler.p436)

;; brute force
;; shows solution is close to 0.4723397
;
; (defn simulate []
;   (loop [s 0.0
;          x nil]
;     (let [x0 (rand)
;           s0 (+ s x0)]
;       (cond
;         (> s0 2) (> x x0)
;         (> s0 1) (if x
;                    (recur s0 x)
;                    (recur s0 x0))
;         :else (recur s0 x)))))
; 
; (defn simulate-many [times]
;   (loop [n 0
;          c 0N]
;     (if (< n (inc times))
;       (if (simulate)
;         (recur (inc n) (inc c))
;         (recur (inc n) c))
;       c)))
; 
; (def batch-size 100000)
; 
; (reduce
;   (fn [[tw t] w]
;     (let [tw (+ tw w)
;           t (+ t batch-size)]
;       (println tw t (double (/ tw t)))
;       [tw t]))
;   [0 0]
;   (pmap simulate-many (repeat batch-size)))

; numerical integration
; http://www.learningclojure.com/2011/05/numerical-integration-better.html
(defn iterated-rule [rule f a b N]
  (if (= N 0)
    (rule f a b)
    (let [midpoint (+ a (/ (- b a) 2))]
      (+ (iterated-rule rule f a midpoint (dec N))
         (iterated-rule rule f midpoint b (dec N))))))

(defn simpson-rule [f a b]
  (let [midpoint (+ a (/ (- b a) 2))]
    (* 1/6 (- b a) (+ (f a) (* 4 (f midpoint)) (f b)))))

(defn fac [n]
  (if (< n 1)
    1
    (reduce * (range 1 (inc n)))))

(defn f [n1 n2 x]
  (* (/ 1 (fac (dec n1)) (fac (dec n2)))
     (+ (* (/ 1 n2 (inc n2))
           (Math/pow x (dec n1))
           (Math/pow (- 1 x) (+ n2 2)))
        (* (/ -1 n2 (+ n2 2))
           (Math/pow x (+ n1 n2 1)))
        (* (/ 1 (inc n2) (+ n2 2))
           (Math/pow x (+ n1 n2 1)))
        (* (/ -1 n2 (inc n2))
           (Math/pow x (dec n1))
           (- 1 x)))))

(defn P [n1 n2]
  (if (zero? n2)
    (/ 1 2 (+ n1 2) (fac (dec n1)))
    (/ 
      (+ (/ (+ (* n2 n2) (* 2 n2) 1)
           (+ n2 n1 2) 
           (+ (* n2 n2) n2)
           (+ (* n2 n2) (* 3 n2) 2)))
      (fac (dec n1))
      (fac (dec n2)))))

(double
(reduce +
  (for [n1 (range 1 10)
        n2 (range 0 2)
        :let [a (P n1 n2)
              _ (println a)]]
    a)))

; (defn P-Sn1<1 [n]
;   (/ 1 (fac n)))
; 
; (defn P-S1n1n2<2|n1<1 [n1 n2]
;   (if (zero? n2)
;     1
;     (let [f (fn [n1 n2 Sn1]
;               (* (/ 1 (fac (dec n1))) 
;                  (Math/pow Sn1 (dec n1))
;                  (/ 1 (fac (inc n2)))
;                  (- (Math/pow (- 2 Sn1) n2) 
;                     (* (Math/pow (- 1 Sn1) n2) (inc n2)))))]
;       (iterated-rule simpson-rule (partial f n1 n2) 0 1 5))))
; 
; (reduce +
; (for [n1 (range 1 4)
;       n2 (range 0 4)]
;   (let [a (* (P-Sn1<1 n1) (P-S1n1n2<2|n1<1 n1 n2))]
;     (println n1 n2 a)
;     a)))

    ; (let [f (if (zero? n2)
    ;           (partial f2' n1)
    ;           (partial f' n1 n2))
    ;       a (iterated-rule simpson-rule f 0 1 5)]
    ;   (println n1 n2 a)
    ;   a)))

;(iterated-rule simpson-rule (partial f' 1) 0 0 10)



