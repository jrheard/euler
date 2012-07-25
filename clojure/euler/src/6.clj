; Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

(require '[clojure.math.numeric-tower :as math])

(let [nums (range 1 101)
      squares (map #(* % %) nums)
      sum (apply + nums)]
  (math/abs (- (apply + squares) (* sum sum))))
