(ns euler.p040)

(defn num-2-seq [n]
  (loop [r (list)
         n n]
    (if (< n 10)
      (conj r n)
      (recur (conj r (mod n 10)) (quot n 10)))))

(def decimals
  ((fn decimals [n]
    (lazy-seq (concat (num-2-seq n) (decimals (inc n))))) 1))

(*
  (nth decimals 0)
  (nth decimals 9)
  (nth decimals 99)
  (nth decimals 999)
  (nth decimals 9999)
  (nth decimals 99999)
  (nth decimals 999999))

