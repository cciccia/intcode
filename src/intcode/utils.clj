(ns intcode.utils)

(defn converge-via
  [f x]
  (reductions
    (fn [x x']
      (if (= x x')
        (reduced x')
        x'))
    (iterate f x)))