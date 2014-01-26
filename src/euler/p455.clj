(ns euler.p455
  (:require [euler.helper :as h]))

; using the hint of f(4) = 411728896, we see that
; 4^6 % 10 = 6
; 4^96 % 100 = 96
; 4^896 % 1000 = 896
; ...
; 4^11728896 % 1e8 = 11728896
; hence we can devise a lazy strategy to search for the answer
; strategy:
; base case search for single digits d exponential such that x^d % 10 = d
; inductive step, for i-digit numbers that satisfy x^d % 10^i = d,
;                   find (i+1)-digit numbers that satisfy x^d % 10^(i+1) = d

(def possible-base-cases [6 7 5 3 9])

#_(defn base-case [n digits]
  (let [modular (int (Math/pow 10 digits))
        largest-possible (dec modular)]
    (loop [i largest-possible]
      (when (> i 0)
        (if (= i (h/pow-mod n i modular))
          i
          (recur (dec i)))))))

(defn base-case [n]
  (let [base (mod n 10)]
    (condp = base
      0 nil
      1 [1]
      2 [2 4 8 6]
      3 [3 9 7 1]
      4 [4 6]
      5 [5]
      6 [6]
      7 [7 9 3 1]
      8 [8 4 2 6]
      9 [9 1])))

(defn get-largest [answers]
  (reduce max answers))

(defn f [n]
  (let [digits (inc (int (Math/log10 n)))
        base-case (base-case n)]
    (if (nil? base-case)
      0
      (loop [digits 1
             answers base-case]
        (if (= digits 9)
          (get-largest answers)
          (let [m1 (int (Math/pow 10 digits))
                m2 (* m1 10)
                new-answers (for [i (range 1 10)
                                  :let [a (* m1 i)]
                                  answer answers
                                  :let [new-num (+ answer a)]
                                        ;_ (println n new-num m2)]
                                  :when (= new-num (h/pow-mod n new-num m2))]
                              new-num)]
            (if (empty? new-answers)
              (get-largest answers)
              (recur (inc digits) new-answers))))))))

;(base-case 92)
;(f 157)

(loop [n 2
       answer 0N]
  (if (> n 1000)
    answer
    (let [a (f n)]
      ;(println n a)
      (recur (inc n) (+ answer a)))))

;(base-case 157)
;(h/pow-mod 157 157 1000)
; looks like for longer digits we need a different test for base case
