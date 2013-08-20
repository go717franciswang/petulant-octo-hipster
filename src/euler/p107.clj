(ns euler.p107)

(def network
  (vec (map
    (fn [line]
      (vec (map 
        (fn [element]
          (if (= element "-")
            nil
            (read-string element)))
        (clojure.string/split line #","))))
    (clojure.string/split-lines (slurp (.getFile (clojure.java.io/resource "network.txt")))))))

(def size (count network))

(def start-edge
  (set (first
    (first
      (sort-by second
        (for [y (range (inc size))
              x (range (inc size))
              :let [v (get-in network [y x])]
              :when v]
          [[y x] v]))))))

; dijkstra? to visit all nodes
(def reduced-val
  (loop [visited-nodes start-edge
         edge-nodes start-edge
         value (get-in network start-edge)]
    (if (= (count visited-nodes) size)
      value
      (let [edge-candidates (for [y edge-nodes
                                  x (range (inc size))
                                  :let [v (get-in network [y x])]
                                  :when (and v 
                                             (not (contains? visited-nodes x)))]
                              [[y x] v])
            [[y x] v] (first (sort-by second edge-candidates))]
        (recur (conj visited-nodes x) (conj edge-nodes x) (+ value v))))))

(def total-val
  (/ (reduce + (filter identity (flatten network))) 2))

(- total-val reduced-val)
