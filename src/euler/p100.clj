(ns euler.p100)

(defn t [b]
  (Math/round (/ (inc (Math/pow (inc (* 8 b (dec b))) 0.5)) 2)))

(defn valid? [[b t]]
  (= (* 2 b (dec b)) (* t (dec t))))

(def a
  (filter
    valid?
    (map
      (fn [b]
        [b (t b)])
      (range 100000))))

; generated the sequence under 100k and search in OEIS 
; http://oeis.org/search?q=1%2C3%2C15%2C85%2C493%2C2871&sort=&language=&go=Search
