(ns euler.p085)

(defn rectangle-count [w h]
  (* 0.25 (inc h) h (inc w) w))

(def close-to 2000000)

(defn w-given-h [h]
  "solve for w given h and rectangle-count = close-to using quadratic formual"
  (let [c (/ (* close-to 4.0) (inc h) h)
        w (/ (dec (Math/pow (inc (* 4 c)) 0.5)) 2)
        wl (Math/floor w)
        wu (Math/ceil w)]
    (if (> (Math/abs (- (rectangle-count wl h) close-to))
           (Math/abs (- (rectangle-count wu h) close-to)))
      wu wl)))

((comp second first)
  (sort-by first
    (for [h (range 1 (inc (w-given-h 1)))
          :let [w (w-given-h h)
                d (Math/abs (- (rectangle-count w h) close-to))
                a (* w h)]]
      [d a])))
