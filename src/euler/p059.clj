(ns euler.p059)

(def cipher
  (map 
    read-string
    (clojure.string/split
      (clojure.string/trim
        (slurp (.getFile (clojure.java.io/resource "cipher1.txt"))))
      #",")))

(def cipher-key
  (map
    #(bit-xor 
      (first (last (sort-by second (frequencies (take-nth 3 (drop % cipher))))))
      (int \space))
    (range 3)))

(def msg
  (flatten 
    (map
      (fn [part]
        (map bit-xor part cipher-key))
      (partition-all 3 cipher))))

(println (apply str (map char msg)))

(reduce + msg)
