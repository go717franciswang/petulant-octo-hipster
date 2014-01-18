(ns euler.p323
  (:require [euler.helper :as h]))

; \sum _{ i=1 }^{ \infty  } i((1-1/2^i)^{23} - (1-1/2^{i-1})^{23})

(loop [i 1
       pi-old 0M
       E 0M]
  (let [pi (with-precision 30 (reduce * (repeat 32 (- 1 (/ 1 (h/big-pow 2 i))))))
        new-E (with-precision 30
                (+ E (* i (- pi pi-old))))]
    (println E)
    (recur (inc i) pi new-E)))
