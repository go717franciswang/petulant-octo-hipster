(ns euler.p458)

(loop [d1 7
       d2 0
       d3 0
       d4 0
       d5 0
       d6 0
       digits 1]
  (if (= digits 7)
    (+ d1 d2 d3 d4 d5 d6)
    (let [nd1 (+ d1 d2 d3 d4 d5 d6)
          nd2 (+ (* d1 6) d2 d3 d4 d5 d6)
          nd3 (+ (* d2 5) d3 d4 d5 d6)
          nd4 (+ (* d3 4) d4 d5 d6)
          nd5 (+ (* d4 3) d5 d6)
          nd6 (+ (* d5 2) d6)]
      (recur nd1 nd2 nd3 nd4 nd5 nd6 (inc digits)))))

