(ns euler.p265)

(def N 5)
(def final-length (int (Math/pow 2 N)))

(def rings
  (loop [candidates [[0 (set (range 1 final-length))]]
         l 1]
    (if (== l final-length)
      (map #(quot (first %) (/ final-length 2)) candidates)
      (recur
        (reduce
          (fn [candidates [n availables]]
            (let [append-0 (bit-shift-left n 1)
                  append-1 (bit-set (bit-shift-left n 1) 0)
                  last-bits-0 (mod append-0 final-length)
                  last-bits-1 (mod append-1 final-length)
                  valid-0? (contains? availables last-bits-0)
                  valid-1? (contains? availables last-bits-1)
                  candidates (if valid-0?
                               (conj candidates [append-0 (disj availables last-bits-0)])
                               candidates)
                  candidates (if valid-1?
                               (conj candidates [append-1 (disj availables last-bits-1)])
                               candidates)]
              candidates))
          []
          candidates)
        (inc l)))))

(reduce + rings)
