(ns euler.p186)

(def S (atom []))
(def caller-callees (atom {}))
(def friends (atom #{524287}))
(def everyone (atom #{}))
(def calls-made (atom 0))
(def new-friends (atom #{}))

(defn gen-s! [k]
  (let [s (if (<= k 55) 
            (mod (+ (- 100003 (* 200003 k)) (* 300007 (* k k k))) 1000000)
            (mod (+ (get @S (- k 25)) (get @S (- k 56))) 1000000))]
    (swap! S conj s)
    (swap! everyone conj s)))

(defn filter-new-friends [people]
  (filter #(not (contains? @friends %)) people))

(defn befriend! [new-friend]
  (swap! new-friends conj new-friend)
  (while (not (empty? @new-friends))
    (let [new-friend (first @new-friends)
          more-friends (filter-new-friends (get @caller-callees new-friend))]
      (swap! new-friends into more-friends)
      (swap! caller-callees dissoc new-friend)
      (swap! friends conj new-friend)
      (swap! new-friends disj new-friend))))

(defn update-caller-callees! [i]
  (let [caller (get @S (- i 2))
        callee (get @S (- i 1))]
    (when (not= caller callee)
      (swap! calls-made inc)
      (let [callees (conj (get @caller-callees caller #{}) callee)]
        (swap! caller-callees #(assoc % caller callees)))
      (when (and (contains? @friends caller) (not (contains? @friends callee)))
        (befriend! callee))
      (when (and (contains? @friends callee) (not (contains? @friends caller)))
        (befriend! caller)))))

(defn make-call! []
  (let [k (inc (count @S))]
    (gen-s! k)
    (gen-s! (inc k))
    (update-caller-callees! (inc k))
    (when (zero? (mod @calls-made 100000))
      (println "made" @calls-made "calls, " (count @friends) "/" (count @everyone) "connected"))))

(while (or (zero? (count @everyone)) (< (/ (count @friends) (count @everyone)) 0.99))
  (make-call!))

(println @calls-made)
