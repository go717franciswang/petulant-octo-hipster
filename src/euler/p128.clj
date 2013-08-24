(ns euler.p128
  (:require [euler.helper :as h]))

(def primes (set (h/primes 1E5)))

(def max-layer 25000)

(def opposite
  (let [ds {:N :S :NW :SE :SW :NE}]
    (reduce
      (fn [ds [d1 d2]]
        (assoc ds d2 d1))
      ds
      ds)))

(def adjacent-chain
  {:NE [:S :SE]
   :N [:SE :NE]
   :NW [:NE :N]
   :SW [:N :NW]
   :S [:NW :SW]
   :SE [:SW :S]})

(defn layer-seq [layer]
  (butlast
    (concat (repeat layer :SW)
            (repeat layer :S)
            (repeat layer :SE)
            (repeat layer :NE)
            (repeat layer :N)
            (repeat layer :NW))))

(def mapping
  (loop [mapping (transient [{}])
         layer 1]
    (if (> layer max-layer)
      (persistent! mapping)
      (let [south-hex-num (if (= layer 1) 
                            1 
                            (- (count mapping) (dec (* 6 (dec layer)))))
            new-hex {:S south-hex-num}
            new-hex-num (inc (count mapping))
            new-south-hex (assoc (get mapping (dec south-hex-num)) :N new-hex-num)
            mapping (assoc! (conj! mapping new-hex) (dec south-hex-num) new-south-hex)
            mapping (reduce
                      (fn [mapping direction]
                        (let [last-hex-num (count mapping)
                              last-hex (get mapping (dec last-hex-num))
                              new-hex-num (inc last-hex-num)
                              backward-direction (opposite direction)
                              [d1 adjacent-direction] (adjacent-chain backward-direction)
                              adjacent-num (last-hex d1)
                              adjacent-hex (get mapping (dec adjacent-num))
                              [d2 adjacent-direction2] (adjacent-chain adjacent-direction)
                              adjacent-num2 (adjacent-hex d2)
                              new-hex (into {} (filter second
                                                       [[backward-direction last-hex-num]
                                                        [adjacent-direction adjacent-num]
                                                        [adjacent-direction2 adjacent-num2]]))
                              new-last-hex (assoc last-hex direction new-hex-num)
                              new-adjacent-hex (assoc adjacent-hex 
                                                      (opposite adjacent-direction) new-hex-num)
                              mapping (assoc! mapping 
                                        (dec new-hex-num) new-hex 
                                        (dec last-hex-num) new-last-hex 
                                        (dec adjacent-num) new-adjacent-hex)]
                          (if adjacent-num2
                            (let [adjacent-hex2 (get mapping (dec adjacent-num2))
                                  new-adjacent2-hex (assoc adjacent-hex2 
                                                           (opposite adjacent-direction2) 
                                                           new-hex-num)]
                              (assoc! mapping (dec adjacent-num2) new-adjacent2-hex))
                            mapping)))
                      mapping
                      (layer-seq layer))
            last-hex (assoc (get mapping (dec (count mapping))) :NW new-hex-num)
            last-hex-num (count mapping)]
        (let [mapping (assoc! mapping (dec last-hex-num) last-hex)
              new-hex (assoc (get mapping (dec new-hex-num)) :SE (count mapping))]
          (recur (assoc! mapping (dec new-hex-num) new-hex) (inc layer)))))))

(defn PD [n]
  (let [neighbors (vals (get mapping (dec n)))
        diffs (map #(biginteger (Math/abs (- n %))) neighbors)]
    (count (filter #(contains? primes %) diffs))))

(loop [tiles (transient [])
       n 1]
  (when (>= n (count mapping))
    (throw (Throwable. "Need more layers")))
  (let [pd (PD n)]
    (if (= pd 3)
      (let [tiles (conj! tiles n)]
        (println (count tiles) n)
        (if (= (count tiles) 2000)
          n
          (recur tiles (inc n))))
      (recur tiles (inc n)))))
