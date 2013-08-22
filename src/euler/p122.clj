(ns euler.p122)

(def cap 200)

; first attempt kept inefficient paths to a node
; while trying to eliminate inefficient path in 2nd attemp, an atom was used keep discovered nodes
; more elegant to implement without an atom
(def result (atom {1 0}))

(defn form-new-nums [nums]
  (let [m (last nums)]
    (distinct
      (for [n1 nums
            n2 nums
            :let [n3 (+ n1 n2)]
            :when (and (> n3 m) (<= n3 cap) (not (contains? @result n3)))]
        n3))))

(loop [regens [[1]]]
  (let [regens (for [regen regens
                     new-num (form-new-nums regen)]
                 (conj regen new-num))
        _ (println "regens count" (count regens))]
    (doseq [regen regens]
      (let [new-num (last regen)]
        (swap! result assoc new-num (dec (count regen))))
      regens)
    (println "found" (count @result))
    (if (= (count @result) cap)
      (reduce + (vals @result))
      (recur (doall regens)))))
