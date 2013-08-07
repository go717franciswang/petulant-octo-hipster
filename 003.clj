(loop [s 600851475143
       d 2]
  (if (= s d)
    d
    (if (= 0 (mod s d))
      (recur (/ s d) d)
      (recur s (inc d)))))
