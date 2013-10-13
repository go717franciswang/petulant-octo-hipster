(ns euler.p185
  (:require [clojure.string :as s]
            [euler.helper :as h]
            [clojure.math.combinatorics :as c]))

(def input "90342 ;2 correct
           70794 ;0 correct
           39458 ;2 correct
           34109 ;1 correct
           51545 ;2 correct
           12531 ;1 correct")
(def input "5616185650518293 ;2 correct
           3847439647293047 ;1 correct
           5855462940810587 ;3 correct
           9742855507068353 ;3 correct
           4296849643607543 ;3 correct
           3174248439465858 ;1 correct
           4513559094146117 ;2 correct
           7890971548908067 ;3 correct
           8157356344118483 ;1 correct
           2615250744386899 ;2 correct
           8690095851526254 ;3 correct
           6375711915077050 ;1 correct
           6913859173121360 ;1 correct
           6442889055042768 ;2 correct
           2321386104303845 ;0 correct
           2326509471271448 ;2 correct
           5251583379644322 ;2 correct
           1748270476758276 ;3 correct
           4895722652190306 ;1 correct
           3041631117224635 ;3 correct
           1841236454324589 ;3 correct
           2659862637316867 ;2 correct")

(def corrects
  (reduce 
    (fn [m line]
      (let [[a b] (s/split line #"\s\;")
            n (read-string a)
            [c _] (s/split b #"\s")
            c (read-string c)]
        (assoc m n c)))
      {}
      (s/split input #"\n")))

(def queue (map first (sort-by second corrects)))

(def len (inc (int (/ (Math/log (first queue)) (Math/log 10)))))

(def init (repeat len (set (range 10))))

(defn valid? [possibilities digits poss-positions]
  (every? 
    (fn [p]
      (contains? (get possibilities p) (get digits p)))
    poss-positions))

(defn set-poss [possibilities digits poss-positions]
  (reduce 
    (fn [possibilities p]
      (assoc possibilities p (hash-set (get digits p))))
    possibilities
    poss-positions))

(defn remove-poss [possibilities digits poss-positions]
  (loop [p 0
         poss-positions poss-positions
         possibilities possibilities]
    (if (= p len)
      possibilities
      (if (= p (first poss-positions))
        (recur (inc p) (rest poss-positions) possibilities)
        (let [poss (get possibilities p)
              d (get digits p)]
          (if (contains? poss d)
            (recur (inc p) poss-positions (update-in possibilities [p] #(disj % d)))
            (recur (inc p) poss-positions possibilities)))))))

; for every number (going from the fewest corrects to most)
;   get a list of possible positions based on the number of corrects
;   for every possible position
;     check if these positions are still valid
;     if not valid continue to next possible position
;     else set these positions as the only possibility
;     remove the rest of positions choices from the choice set
;     recur to next number in the queue
(defn guess [possibilities queue]
  (let [possibilities (vec possibilities)]
    (cond
      (some empty? possibilities) nil
      (empty? queue) (do (println possibilities) possibilities)
      :else (let [n (first queue)
                  digits (vec (h/num-2-seq n))
                  c (get corrects n)]
              (if (zero? c)
                (recur (map #(disj %1 %2) possibilities digits) (rest queue))
                (let [answer (apply concat
                               (for [poss-positions (c/combinations (range len) c)
                                     :when (valid? possibilities digits poss-positions)]
                                 (let [new-poss (set-poss possibilities digits poss-positions)
                                       new-poss (remove-poss new-poss digits poss-positions)
                                       result (guess new-poss (rest queue))]
                                   (when (not (empty? result)) result))))]
                  (filter identity answer)))))))

; not sure how long to complete
; terminate script as soon as one solution is printed since we know it's unique
(time (guess init queue))
