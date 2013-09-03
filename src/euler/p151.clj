(ns euler.p151)

(def transform
  {:A2 [:A3 :A4 :A5]
   :A3 [:A4 :A5]
   :A4 [:A5]
   :A5 []})

(defn add [left more]
  (reduce 
    (fn [left x]
      (update-in left [x] inc))
    left
    more))

(defn left-count [left]
  (reduce + (map second left)))

(loop [probabilities [{:prob 1.0 :left {:A2 1 :A3 1 :A4 1 :A5 1} :s 0}]]
  (println (count probabilities))
  (let [probabilities 
         (for [{:keys [prob left s]} probabilities
               :let [c (left-count left)
                     new-s (if (= c 1) (inc s) s)]
               [pick m] left
               :when (> m 0)
               :let [p (/ (* prob m) c)
                     more-left (transform pick)
                     new-left (add (update-in left [pick] dec) more-left)]]
                     ;_ (println left)]]
           {:prob p :left new-left :s new-s})]
    (if (= (:left (first probabilities)) {:A2 0 :A3 0 :A4 0 :A5 1})
      (reduce +
        (map
          (fn [{:keys [prob s]}]
            (* prob s))
          probabilities))
      (recur probabilities))))

