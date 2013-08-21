(ns euler.p114)

(def min-block-size 3)

(def total-length 50)

(defn blocks [max-size]
  (cons [:B 1] (map #(vector :R %) (range min-block-size (inc max-size)))))

(def result-init (map 
                   (fn [[_ block-size :as block]]
                     [(- total-length block-size) [block]])
                   (blocks (dec total-length))))

(inc (count
  (loop [regen result-init
         results []]
    (println "regens count: " (count regen))
    (if (empty? regen)
      results
      (let [regen-and-results (for [[left block-seq :as result] regen
                                    :let [[last-block-color last-block-size] (last block-seq)]
                                    [color size :as next-block] (blocks left)
                                    :while (or (= last-block-color :B) (= color :B))
                                    :let [new-left (- left size)]
                                    :when (>= new-left 0)]
                                (if (= last-block-color color :B)
                                  (let [updated-block [:B (+ last-block-size size)]
                                        last-idx (dec (count block-seq))
                                        new-block-seq (assoc block-seq last-idx updated-block)]
                                    [new-left new-block-seq])
                                  [new-left (conj block-seq next-block)]))
            [regen more-results] (reduce
                                   (fn [[regen more-results] [left _ :as result]]
                                     (if (zero? left)
                                       [regen (conj more-results result)]
                                       [(conj regen result) more-results])) 
                                   [[] []]
                                   regen-and-results)]
        (recur regen (concat results more-results)))))))
                                  
