(ns euler.p098
  (:require [clojure.math.combinatorics :as combo]))

(def words
  (map
    #(subs % 1 (dec (count %)))
    (clojure.string/split
      (slurp (.getFile (clojure.java.io/resource "words.txt")))
      #",")))

(def at-least-4-char-anagrams
  (filter #(and (> (count %) 1) (>= (count (first %)) 4))
    (map second
      (group-by sort words))))

(def anagram-pairs
  (for [anagrams at-least-4-char-anagrams
        pair (combo/combinations anagrams 2)]
    pair))

(defn word-2-int [word char-map]
  (let [s (apply str (map char-map word))]
    (if (= (subs s 0 1) "0")
      (throw (Exception. "Invalid char-map"))
      (read-string s))))

(defn length-n-ints [n]
  (for [vs (combo/combinations (range 10) n)
        v (combo/permutations vs)]
    v))

(defn unique-char-count [word]
  (count (distinct word)))

(defn square? [x]
  (let [a (int (Math/pow x 0.5))]
    (= x (* a a))))

(defn anagram-square [w1 w2]
  (let [n (unique-char-count w1)]
    (filter identity
      (for [v (length-n-ints n)]
        (try
          (let [m (zipmap w1 v)
                i1 (word-2-int w1 m)
                i2 (word-2-int w2 m)]
            (if (every? square? [i1 i2])
              [i1 i2]
              nil))
          (catch Exception e
            nil))))))

(defn max-anagram-square [w1 w2]
  (reduce max 0 (flatten (anagram-square w1 w2))))

(reduce max
  (for [[w1 w2] anagram-pairs]
    (let [a (max-anagram-square w1 w2)]
      (println [w1 w2] a)
      a)))
