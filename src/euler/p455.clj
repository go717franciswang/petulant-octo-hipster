(ns euler.p455
  (:require [euler.helper :as h]))

; using the hint of f(4) = 411728896, we see that
; 4^6 % 10 = 6
; 4^96 % 100 = 96
; 4^896 % 1000 = 896
; ...
; 4^11728896 % 1e8 = 11728896
; hence we can devise a lazy strategy to search for the answer
