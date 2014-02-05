(ns euler.p209)

; so that's why they named it circular logic: http://garethrees.org/2013/05/15/euler/

(defn get-new-node [node-seen]
  (loop [i 0]
    (cond 
      (= i 64) nil
      (contains? node-seen i) (recur (inc i))
      :else i)))

(defn get-next-node [node]
  (let [next-node (bit-clear (bit-shift-left node 1) 6)
        a (if (bit-test node 5) 1 0)
        b (if (bit-test node 4) 1 0)
        c (if (bit-test node 3) 1 0)]
    (if (zero? (bit-xor a (bit-and b c)))
      next-node
      (bit-set next-node 0))))

(def circles
  (loop [circles [[0]]
         node-seen #{0}]
    (let [node (-> circles last last)
          next-node (get-next-node node)]
      (if (contains? node-seen next-node)
        (let [new-node (get-new-node node-seen)]
          (if new-node
            (recur (conj circles [new-node]) (conj node-seen new-node))
            circles))
        (recur (update-in circles [(dec (count circles))] conj next-node) 
               (conj node-seen next-node))))))

; solve with dp
(defn get-combinations [circle]
  (let [nodes (count circle)
        start-true (loop [last-true 1
                          last-false 0
                          nodes-left (dec nodes)]
                     (if (zero? nodes-left)
                       last-false
                       (recur last-false (+ last-true last-false) (dec nodes-left))))
        start-false (loop [last-true 0
                           last-false 1
                           nodes-left (dec nodes)]
                      (if (zero? nodes-left)
                        (+ last-true last-false)
                        (recur last-false (+ last-true last-false) (dec nodes-left))))]
    (+ start-true start-false)))

(reduce * (map get-combinations circles))

