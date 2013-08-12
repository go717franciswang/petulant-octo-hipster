(ns euler.p055)

(defn palindrome? [n]
  (let [s (str n)
        l (/ (count s) 2)]
    (= (take l s) (reverse (take-last l s)))))
      
(defn reverse-digits [n]
  (read-string (apply str (drop-while #(= % \0) (reverse (str n))))))

(defn lychrel? [n]
  (loop [tries 50
         n (bigint n)]
    (if (zero? tries)
      true
      (let [new-n (+ n (reverse-digits n))]
        (if (palindrome? new-n)
          false
          (recur (dec tries) new-n))))))

(count (filter lychrel? (range 10 10000)))
