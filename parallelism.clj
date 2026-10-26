(ns parallelism)

(defn bits [x]
  (.bitCount (biginteger x)))

(defn fact-seq [n]
  (loop [iteration 1
         result    1]
    (if (< n iteration)
      (bits result)
      (recur (inc iteration)
             (*' result iteration)))))

(defn fact-partial [[start end]]
  (loop [iteration start
         result    1]
    (if (= iteration end)
      result
      (recur (inc iteration)
             (*' result iteration)))))

(range 1 100 (quot 100 8))

(defn fact-ranges [n p]
  (->> (quot n p)
       (range 1 n)
       (vec)
       (#(conj % (inc n)))
       (partition 2 1)))

(defn fact-par [n]
  (let [p (.availableProcessors (Runtime/getRuntime))]
    (bits (reduce *' (pmap fact-partial (fact-ranges n p))))))

(time (fact-seq 150000))
(time (fact-par 150000))

(defn create-random-data [n]
  (vec (repeatedly n #(rand-int 1000))))

(defn insert-sort [vx]
  (loop [xs vx
         ys []]
    (if (empty? xs)
      ys
      (let [x (first xs)
            [before after] (split-with #(< % x) ys)]
        (recur (rest xs) (concat before [x] after))))))

(defn merge-sort [va vb]
  (loop [as va
         bs vb
         xs []]
    (cond
      (empty? as) (concat xs bs)
      (empty? bs) (concat xs as)
      (< (first as) (first bs)) (recur (rest as) bs (conj xs (first bs)))
      :else (recur as (rest bs) (conj xs (first bs))))))

(defn hybrid-seq [s]
  (if (< (count s) 100)
    (insert-sort s)
    (let [[a b] (split-at (quot (count s) 2) s)]
      (merge-sort (hybrid-seq a)
                  (hybrid-seq b)))))

(defn hybrid-par [s]
  (if (< (count s) 100)
    (insert-sort s)
    (let [split (split-at (quot (count s) 2) s)]
      (apply merge-sort (pmap hybrid-par split)))))

(defn hybrid-fut [s]
  (if (< (count s) 100)
    (insert-sort s)
    (let [[lh rh] (split-at (quot (count s) 2) s)
          lf (future (hybrid-fut lh))
          rf (future (hybrid-fut rh))]
      (merge-sort @lf @rf))))

(defn merge-ranges [s]
  (let [p (.availableProcessors (Runtime/getRuntime))
        n (count s)]
    (->> (conj (vec (range 0 (dec n) (quot n p))) (dec n))
         (partition 2 1)
         (map (fn [[start end]] (subvec s start end))))))

(defn p-chunk-sort [s]
  (reduce merge-sort (pmap hybrid-seq (merge-ranges s))))

(time (apply <= (hybrid-seq (create-random-data 800000))))
(time (apply <= (hybrid-par (create-random-data 800000))))
(time (apply <= (hybrid-fut (create-random-data 800000))))
;; faster than broken above ha
(time (apply <= (p-chunk-sort (create-random-data 800000))))
