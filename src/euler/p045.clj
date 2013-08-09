(ns euler.p045)

(defn h [n]
  (* n (dec (* n 2))))

(defn t? [x]
  (let [n (- (Math/pow (+ 0.25 (* 2 x)) 0.5) 0.5)]
    (= (float (int n)) n)))

(defn p? [x]
  (let [n (/ (+ 0.5 (Math/pow (+ 0.25 (* 6 x)) 0.5)) 3)]
    (= (float (int n)) n)))

(first 
  (filter 
    #(and
       (t? %)
       (p? %))
    (map h (drop 144 (range)))))
