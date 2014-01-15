(ns euler.p186)

; for every call
; if both caller and callee are new, put them in a set, assign the set id to both items
; if only one of them have been seen, put the new one in the existing set, and assign the set id
; if both have been seen, combine 2 sets and store using lower of set id, and assign the set id
(def S (int-array 55 0))
;(def S (atom []))
(def sets (atom [1]))
(def set2set (int-array (range 1000000)))
(def person2set (int-array 1000000 -1))
(aset ^ints person2set 524287 0)
(def calls-made (atom 0))
(def people-seen (atom 1))

(defn connected-ratio [] 
  (/ (first @sets) 1000000))
  ;(/ (first @sets) @people-seen))

(defn get-idx [k]
  (mod (dec k) 55))

(defn gen-s! [k]
  (let [s (if (<= k 55) 
            (mod (+ (- 100003 (* 200003 k)) (* 300007 (* k k k))) 1000000)
            (mod (+ (aget ^ints S (get-idx (- k 24))) (aget ^ints S (get-idx (- k 55)))) 1000000))
        s (int s)]
    (aset ^ints S (get-idx k) s)
    s))

(defn get-set-id [s]
  (let [set-id (aget ^ints person2set s)]
    (loop [set-id set-id]
      (let [new-set-id (aget ^ints set2set set-id)]
        (if (= set-id new-set-id)
          set-id
          (recur new-set-id))))))

(defn combine-sets! [s1 s2]
  (let [set-id1 (get-set-id s1)
        set-id2 (get-set-id s2)
        set1 (get @sets set-id1)
        set2 (get @sets set-id2)
        combined-set (+ set1 set2)
        new-set-id (int (min set-id1 set-id2))]
    (when (not= set-id1 set-id2)
      (swap! sets assoc new-set-id combined-set)
      (aset ^ints set2set set-id1 new-set-id)
      (aset ^ints set2set set-id2 new-set-id))))

(defn add-to-set! [from to]
  (let [to-set-id (int (get-set-id to))
        new-set (inc (get @sets to-set-id))]
    (aset ^ints person2set from to-set-id)
    (swap! sets assoc to-set-id new-set)))

(defn add-new-set! [s1 s2]
  (let [set-id (count @sets)
        new-set 2]
    (aset ^ints person2set s1 set-id)
    (aset ^ints person2set s2 set-id)
    (swap! sets conj new-set)))

(defn make-call! [k]
  (let [caller (gen-s! k)
        callee (gen-s! (inc k))
        seen-caller? (not= (aget ^ints person2set caller) -1)
        seen-callee? (not= (aget ^ints person2set callee) -1)]
    (when (not= caller callee)
      (when (not seen-caller?) (swap! people-seen inc))
      (when (not seen-callee?) (swap! people-seen inc))
      (swap! calls-made inc)
      (cond
        (and seen-caller? seen-callee?) (combine-sets! caller callee)
        seen-caller? (add-to-set! callee caller)
        seen-callee? (add-to-set! caller callee)
        :else (add-new-set! caller callee)))))

(loop [k 1]
  (when (or (zero? @calls-made) (< (connected-ratio) 0.99))
    (when (zero? (mod @calls-made 100000))
      (println "made" @calls-made "calls," (count @sets) "sets," (float (connected-ratio)) "connected"))
    (make-call! k)
    (recur (+ k 2))))

(println @calls-made)
