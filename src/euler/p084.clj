(ns euler.p084)

; note: will be more elegant if written without atoms
; markov chain is probably more elegant than monte carlo

(def game-board 
  [:GO :A1 :CC1 :A2 :T1 :R1 :B1 :CH1 :B2 :B3 :JAIL :C1 :U1 :C2 :C3 :R2 :D1 :CC2 :D2 :D3 
   :FP :E1 :CH2 :E2 :E3 :R3 :F1 :F2 :U2 :F3 :G2J :G1 :G2 :CC3 :G3 :R4 :CH3 :H1 :T2 :H2])
(def square-to-digit
  (reduce
    (fn [m [k v]]
      (assoc m v k))
    {}
    (map-indexed #(vector % %2) game-board)))

(def board (atom (cycle game-board)))
(def chance? #{:CH1 :CH2 :CH3})
(def community-chest? #{:CC1 :CC2 :CC3})
(def go-to-jail? (partial = :G2J))
(def rail-way? #{:R1 :R2 :R3 :R4})
(def utility-company? #{:U1 :U2})

(defn go-to [square]
  (condp = square
    :next-R (swap! board (partial drop-while (comp not rail-way?)))
    :next-U (swap! board (partial drop-while (comp not utility-company?)))
    :back-3 (swap! board (partial drop (- 40 3)))
    (swap! board (partial drop-while (partial not= square)))))

(defn go-forward [n]
  (swap! board (partial drop n)))

(def community-chest-cards 
  (atom 
    (cycle (shuffle (list* #(go-to :GO) 
                           #(go-to :JAIL) 
                           (repeat 14 nil))))))
(defn draw-from-community-chest []
  (let [f (first @community-chest-cards)]
    (swap! community-chest-cards rest)
    f))

(def chance-cards 
  (atom 
    (cycle (shuffle (list* #(go-to :GO)
                           #(go-to :JAIL)
                           #(go-to :C1)
                           #(go-to :E3)
                           #(go-to :H2)
                           #(go-to :R1)
                           #(go-to :next-R)
                           #(go-to :next-R)
                           #(go-to :next-U)
                           #(go-to :back-3)
                           (repeat 6 nil))))))
(defn draw-from-chance-cards []
  (let [f (first @chance-cards)]
    (swap! chance-cards rest)
    f))

(defn roll-dices []
  (+ 2 (rand-int 4) (rand-int 4)))

(def result-size 1000000)

(let [square-count ((comp reverse (partial sort-by second) frequencies)
                     (loop [results []
                            last-3-roll '(1 1 1)]
                       (if (>= (count results) result-size)
                         results
                         (let [m (roll-dices)]
                           (if (every? false? last-3-roll)
                             (do
                               (go-to :JAIL)
                               (recur (conj results (first @board)) '(1 1 1)))
                             (let [square (first (go-forward m))
                                   move-again (cond 
                                                (chance? square) (draw-from-chance-cards)
                                                (community-chest? square) (draw-from-community-chest)
                                                (go-to-jail? square) #(go-to :JAIL)
                                                :else nil)]
                               (when move-again (move-again))
                               (recur (conj results (first @board)) (conj (butlast last-3-roll) m))))))))
      total (reduce + (map second square-count))]
  (map (fn [[square c]] [square (square square-to-digit) (float (/ c total))]) square-count))
