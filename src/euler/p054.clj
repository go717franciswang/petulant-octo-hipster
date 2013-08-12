(ns euler.p54)

(defn read-card [[rank suit]]
  {:rank (get {\2 2
               \3 3
               \4 4
               \5 5
               \6 6
               \7 7
               \8 8
               \9 9
               \T 10
               \J 11
               \Q 12
               \K 13
               \A 14} rank)
   :suit suit})

(def hands
  (map
    #(split-at 5 (map read-card (clojure.string/split % #"\s")))
    (clojure.string/split 
      (slurp (.getFile (clojure.java.io/resource "poker.txt")))
      #"\n")))

(defn one-pair? [h]
  (some #(= (second %) 2) (frequencies (map :rank h))))

(defn two-pair? [h]
  (= 2 (count (filter #(= (second %) 2) (frequencies (map :rank h))))))

(defn three-of-a-kind? [h]
  (some #(= (second %) 3) (frequencies (map :rank h))))

(defn straight? [h]
  (let [ranks (sort (map :rank h))
        a (first ranks)
        b (last ranks)]
    (or
      (= ranks (range a (inc b)))
      (and (= b 14) (= (butlast ranks) (range 2 6))))))

(defn flush? [h]
  (apply = (map :suit h)))

(defn full-house? [h]
  (and (one-pair? h) (three-of-a-kind? h)))

(defn four-of-a-kind? [h]
  (some #(= (second %) 4) (frequencies (map :rank h))))

(defn straight-flush? [h]
  (and (straight? h) (flush? h)))

(defn royal-flush? [h]
  (and (straight-flush? h)
       (= 14 (reduce max (map :rank h)))))

(defn score [h]
  (let [ranks (map :rank h)]
    (cond 
      (royal-flush? h) 100000000
      (straight-flush? h) (+ 10000000 (reduce max ranks))
      (four-of-a-kind? h) (+ 1000000 (first ranks))
      (full-house? h) 1000000
      (flush? h) 100000
      (straight? h) 10000
      (three-of-a-kind? h) 1000
      (two-pair? h) 100
      (one-pair? h) 10
      :else (/ (reduce max ranks) 15.0))))

(defn largest-rank [ranks]
  (let [n-of-a-kind (reduce max (map second (frequencies ranks)))
        ranks-with-n (filter #(= (second %) n-of-a-kind) (frequencies ranks))]
    (reduce max (map first ranks-with-n))))


(defn winner [h1 h2]
  (let [s1 (score h1)
        s2 (score h2)]
    (if (= s1 s2)
      (if (> (largest-rank (map :rank h1)) (largest-rank (map :rank h2)))
        h1
        h2)
      (if (> s1 s2)
        h1
        h2))))

(reduce +
  (for [[h1 h2] hands]
    (if (= h1 (winner h1 h2))
      1
      0)))
