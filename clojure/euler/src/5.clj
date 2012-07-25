; 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

(defn evenly-divisible
  [num divisors]
  (every? true? (map #(= (mod num %) 0) divisors)))

(defn nums-evenly-divisible-by
  [divisors]
  ((fn divisible-seq
    [n]
    (lazy-seq
      (let [seq (divisible-seq (+ n 1))]
        (if (evenly-divisible n divisors)
          (cons n seq)
          seq))))
    (last divisors)))

(first (nums-evenly-divisible-by (range 1 11)))
