(ns euler.p019
  (:require [clj-time.core :as ct]))

(def sdate (ct/date-time 1901 1 1))
(def edate (ct/date-time 2000 12 31))

(count
  (filter 
    #(and 
       (= 7 (ct/day-of-week %)) 
       (= 1 (ct/day %)))
    (take-while #(not (ct/after? % edate)) 
                (iterate #(ct/plus % (ct/days 1)) sdate))))
