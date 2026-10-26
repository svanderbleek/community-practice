(ns ch
  (:require [clojure.core.async :as async :refer [<! >! <!! >!!]]))

(defn slice [nums size n]
  (subvec nums
          (* size n)
          (min (* size (inc n))
               (count nums))))

(defn sum-go [nums threads]
  (let [size   (quot (count nums) threads)
        result (async/chan)]
    (dotimes [n threads]
      (let [s (slice nums size n)]
        (async/go (>! result (reduce + s)))))
    (async/put! result (reduce + (slice nums size threads)))
    (loop [sum 0
           n   0]
      (if (<= n threads)
        (recur (+ sum (<!! result)) (inc n))
        sum))))

(defn run-exp [n threads]
  (let [nums (vec (range n))
        start (System/nanoTime)
        sum (sum-go nums threads)
        end (System/nanoTime)]
    (printf "sum of %d numbers is %d\n" n sum)
    (printf "with %d threads took %f time\n" threads (/ (- end start) 1000000.))))

(let [start (System/nanoTime)
      end   (System/nanoTime)]
  (/ (- end start) 1000000.))

(run-exp 10000000 7)
