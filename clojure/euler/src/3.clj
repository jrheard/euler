; The prime factors of 13195 are 5, 7, 13 and 29.
; What is the largest prime factor of the number 600851475143 ?

(defn find-first [pred seq]
  (cond
    (empty? seq) nil
    (pred (first seq)) (first seq)
    :else (find-first pred (rest seq))))

(defn factors
  [num]
  (cond
    (= num 0) '(0)
    (= num 1) '(1)
    :else (let [next-factor (find-first #(= (mod num %) 0) (range 2 (+ num 1)))]
            (cons next-factor (factors (/ num next-factor))))))

(apply max (factors 600851475143))
