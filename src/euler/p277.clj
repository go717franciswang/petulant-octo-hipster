(ns euler.p277)

(def s (seq "DdDddUUdDD"))
(def s (seq "UDDDUdddDDUDDddDdDddDDUDDdUUDd"))
(def s' (reverse s))
(def floor 1E6)
(def floor 1E15)

(defn D' [n]
  (* n 3))

(defn d' [n]
  (/ (inc (* n 3)) 2))

(defn U' [n]
  (/ (- (* n 3) 2) 4))

(defn run [n]
  (loop [a n
         s' s']
    (if (empty? s')
      a
      (let [f (condp = (first s')
                \D D'
                \d d'
                \U U')]
        (recur (f a) (rest s'))))))

(defn D [n]
  (/ n 3))
(defn d [n]
  (/ (dec (* n 2)) 3))
(defn U [n]
  (/ (+ (* n 4) 2) 3))

#_(loop [n 19]
  (println n)
  (when (> n 1)
    (condp = (mod n 3)
      0 (recur (D n))
      1 (recur (U n))
      2 (recur (d n)))))

#_(loop [candidates #{1}]
  (if (> (count candidates) 99)
    (println (sort candidates))
    (recur
      (into candidates
        (for [c candidates
              c1 [(D' c) (d' c) (U' c)]
              :when (not (ratio? c1))] c1)))))



#_(loop [candidates #{1}
       previous-candidates #{}
       answers []]
  (println (count candidates) (count answers))
  (if (or (> (count answers) 2000) (empty? candidates))
    (reduce min (remove ratio? (map run answers)))
    (let [more-candidates (for [c candidates
                                c1 [(D' c) (d' c) (U' c)]
                                :when (and (not (contains? previous-candidates c1)) (not (ratio? c1)))]
                            c1)
          new-candidates (set (filter #(<= (run %) floor) more-candidates))]
      (recur new-candidates candidates (concat answers (filter #(let [a (run %)] (and (> a floor) (not (ratio? a)))) more-candidates))))))

(def a0 
  (loop [a0 (bigint (/ floor (run 1)))
         stop-a 0N]
    (let [a1 (bigint (run a0))]
      (if (= a1 stop-a)
        a0
        (recur (bigint (* a0 (/ floor a1))) a1)))))

(loop [a1 a0]
  (let [a2 (run a1)]
    (println a1 (bigint a2))
    (cond 
      (< a2 floor) (recur (inc a1))
      (ratio? a2) (recur (inc a1))
      :else a2)))


