(ns euler.p104)

(def preserve-last-n-digits 9)

(def overflow-at
  (reduce * (repeat preserve-last-n-digits (bigint 10))))

(defn truncate-num [n]
  (if (< n overflow-at)
    n
    (let [truncated (- n (* (quot n overflow-at) overflow-at))]
      (if (zero? truncated)
        overflow-at
        truncated))))

(def fib-tail
  (memoize
    (fn [n]
      (if (< n 3)
        1
        (truncate-num (+ (fib-tail (dec n)) (fib-tail (- n 2))))))))

(defn ten [n] (.pow (.toBigInteger 10N) n))

(defn truncate-head [n]
  (read-string (apply str (take 15 (str n)))))

(def fib-head
  (memoize
    (fn [n]
      (if (< n 3)
        [1 1]
        (let [[head1 digits1] (fib-head (dec n))
              [head2 digits2] (fib-head (- n 2))
              h1-length (count (str head1))
              h2-length (count (str head2))
              length-diff (- (- digits1 h1-length) (- digits2 h2-length))
              fib (if (pos? length-diff)
                    (+ (* head1 (ten length-diff)) head2)
                    (+ head1 (* head2 (ten (- length-diff)))))
              fib-str (str fib)
              l (count fib-str)
              fib-digits (+ digits1 (- l (+ h1-length length-diff)))
              fib-truncated (truncate-head fib)]
          [fib-truncated fib-digits])))))

(def one2nine (seq (apply str (range 1 10))))

(defn pandigital? [nums-str]
  (= (sort nums-str) one2nine))

(loop [i 1]
  (let [tail (str (fib-tail i))
        [head fib-digits] (fib-head i)
        head (apply str (take 9 (str head)))]
    (if (and
          (pandigital? head)
          (pandigital? tail))
      i
      (recur (inc i)))))
