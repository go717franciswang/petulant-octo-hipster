(ns euler.p243
  (:require euler.helper))

(def primes (apply sorted-set (euler.helper/primes 1E4)))

(def floor (/ 15499.0 94744))
(println "goal: " floor)

(defn prime-factors [n]
  (if (contains? primes n)
    [n]
    (let [stop-at (Math/pow n 0.5)]
      (loop [n n
             results #{}
             p primes]
        (let [p1 (first p)]
          (if (or (> p1 stop-at) (= n 1))
            results
            (if (zero? (mod n p1))
              (recur (quot n p1) (conj results p1) p)
              (recur n results (rest p)))))))))

(loop [p primes
       product 1.0
       dec-product 1.0]
  (let [p1 (first p)
        product (* product p1)
        dec-product (* dec-product (dec p1))
        r (/ dec-product (dec product))]
    (println [(bigint product) p1 r])
    (if (< (Math/abs (- r floor)) 0.0001)
      (first
        (drop-while 
          #(> (second %) floor)
          (map (fn [m] 
                 [m (/ (* m dec-product) (dec (* m product))) (bigint (* m product))]) 
               (range 2 p1))))
      (recur (rest p) product dec-product))))
