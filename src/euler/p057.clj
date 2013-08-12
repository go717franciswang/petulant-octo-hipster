(ns euler.p057)

(def sqrt-2 
  (memoize
    (fn [precision]
      (if (zero? precision)
        (/ 3 2)
        (inc (/ 1 (+ 1 (sqrt-2 (dec precision)))))))))

(defn digits [n]
  (count (str n)))

(count 
  (filter 
    (fn [precision]
      (let [ratio (sqrt-2 precision)] 
        (> (digits (numerator ratio)) (digits (denominator ratio))))) 
    (range 1000)))
