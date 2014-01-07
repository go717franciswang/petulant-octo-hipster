(ns euler.p250
  (:require [euler.helper :as h]))

;; we start off with n numbers = 0 mod 250, there are S[0]=2^n subsets including the empty set
;; next we move onto numbers = 1 mod 250, 
;;   when we include 1 of such number, S[0+1] = S[0]
;;   when we include another of such number, S[0+1] += S[0], S[1+1] = S[1]
;;   when we include yet another of such number, S[0+1] += S[0], S[1+1] += S[1], S[2+1] += S[2]
;;   ...
;; next we move onto numbers = 2 mod 250,
;;   ...
;;   when we include 1 of such number, S[0+2] += S[0], S[1+2] += S[1], ..., S[(249+2)%250] += S[249]
;;   ...
;; ...
;; we move onto numbers = i mod 250,
;;   ...
;;   when we include 1 of such number, S[0+i] += S[0], ..., S[(k+i)%250] += S[k], ...
;;   ...

(def cap 250250)

(def freq
  (let [freq (vec (repeat 250 0))]
    (reduce (fn [freq sum]
              (update-in freq [sum] inc))
            freq
            (pmap #(h/pow-mod % % 250) (range 1 (inc cap))))))

(def modular (h/big-pow 10 16))

(def init-S
  (assoc (vec (repeat 250 0)) 0 (mod (h/big-pow 2 (get freq 0)) modular)))

(dec
  (loop [i 1
         S init-S]
    (println i)
    (if (> i 249)
      (first S)
      (recur (inc i)
             (loop [j 1
                    S S]
               (if (> j (get freq i))
                 S
                 (recur (inc j)
                        (loop [k 0
                               S0 S]
                          (if (> k 249)
                            S0
                            (recur (inc k)
                                   (let [sum (mod (+ k i) 250)]
                                     (assoc S0 sum (mod (+ (get S sum) (get S k)) modular)))))))))))))
