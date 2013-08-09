(ns euler.p042)

(def words
  (map
    #(subs % 1 (dec (count %)))
    (clojure.string/split
      (slurp (.getFile (clojure.java.io/resource "words.txt")))
      #",")))

(defn char-2-num [c]
  (- (int c) 64))

(defn word-2-num [w]
  (reduce + (map char-2-num w)))

(def triangle-nums
  (set (take 100 (map #(int (* 0.5 % (inc %))) (rest (range))))))

(count (filter #(contains? triangle-nums (word-2-num %)) words))
