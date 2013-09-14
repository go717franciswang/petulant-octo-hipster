(ns euler.p172)

(def combos 
  (memoize
    (fn [{:keys [one two three] :as setting} digits-left]
      (if (zero? digits-left)
        1
        (+
          (if (> one 0)
            (* one (combos (conj setting [:one (dec one)]) (dec digits-left)))
             0)
          (if (> two 0)
            (* two (combos (conj setting [:two (dec two)] [:one (inc one)]) (dec digits-left)))
            0)
          (* three (combos (conj setting [:three (dec three)] [:two (inc two)]) (dec digits-left))))))))

(* 9 (combos {:one 0 :two 1 :three 9} 17))
