(ns euler.p458)

; let dn be number of strings with last n characters that are all distinct
; for a string with an extra character, all dn just need to repeat last character to build nd1
;   ...
(def modular (long 1E9))

; going to take a month to finish. haha
(time
(loop [d1 7
       d2 0
       d3 0
       d4 0
       d5 0
       d6 0
       digits 1]
  (when (zero? (mod digits 1000000))
    (let [v [d1 d2 d3 d4 d5 d6]]
      (println (/ digits 1000000000000) (mod (reduce + v) modular) v)))
  (if (= digits 1000000000000)
    (mod (+ d1 d2 d3 d4 d5 d6) modular)
    (let [nd1 (long (mod (+ d1 d2 d3 d4 d5 d6) modular))
          nd2 (long (mod (+ (* d1 6) d2 d3 d4 d5 d6) modular))
          nd3 (long (mod (+ (* d2 5) d3 d4 d5 d6) modular))
          nd4 (long (mod (+ (* d3 4) d4 d5 d6) modular))
          nd5 (long (mod (+ (* d4 3) d5 d6) modular))
          nd6 (long (mod (+ (* d5 2) d6) modular))]
      (recur nd1 nd2 nd3 nd4 nd5 nd6 (inc digits))))))
