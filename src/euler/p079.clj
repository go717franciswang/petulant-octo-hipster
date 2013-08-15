(ns euler.p079)

(def keylog
  (distinct 
    (clojure.string/split
      (slurp (.getFile (clojure.java.io/resource "keylog.txt")))
      #"\n")))

(defn first-digit [keylog]
  (filter 
    (fn [i]
      (let [s (str i)]
        (and
          (every? #(< (.indexOf % s) 1) keylog)
          (some #(= 0 (.indexOf % s)) keylog)))) 
    (range 10)))

(defn remove-first-digit [keylog d]
  (filter
    #(not= % "")
    (map
      #(if (= 0 (.indexOf % (str d)))
        (subs % 1)
        %)
      keylog)))

(loop [keylog keylog
       result []]
  (if (empty? keylog)
    result
    (let [a (first-digit keylog)]
      (if (= 1 (count a))
        (recur (remove-first-digit keylog (first a)) (conj result (first a)))
        [result keylog]))))
