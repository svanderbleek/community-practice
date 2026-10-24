(ns markov
  (:require [clojure.string :as str]))

(defonce ex1 (slurp "ex1.txt"))

(defn markov-data-illegible [text]
  (let [maps
        (for [line (str/split text #"\.")
              m (let [l (str line ".")
                      words
                      (cons :start (str/split l #"\s+"))]
                  (for [p (partition 2 1 (remove #(= "" %) words))]
                    {(first p) [(second p)]}))]
          m)]
    (apply merge-with concat maps)))

(:start (markov-data-illegible ex1))

(defn sentence-illegible [data]
  (loop [ws (data :start)
         acc []]
    (let [w (rand-nth ws)
          nws (data w)
          nacc (concat acc [w])]
      (if (= \. (last w))
        (str/join " " nacc)
        (recur nws nacc)))))

(sentence-illegible (markov-data-illegible ex1))

(defn ->pairs [line]
  (->> (str/split (str line ".") #"\s+")
       (cons :start)
       (remove (fn [w] (= "" w)))
       (partition 2 1)
       (map (fn [p] {(first p) [(second p)]}))))

(first (map ->pairs (str/split ex1 #"\.")))

(defn ->markov [text]
  (->> (str/split text #"\.")
       (mapcat ->pairs)
       (apply merge-with concat)))

(:start (->markov ex1))

(defn take-inclusive [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (cons (first s) (when (pred (first s))
                       (take-inclusive pred (rest s)))))))

(defn ->sentence [markov]
  (->> (rand-nth (:start markov))
       (iterate (fn [w] (rand-nth (markov w))))
       (take-inclusive (fn [w] (not (= \. (last w)))))
       (str/join " ")))

(->sentence (->markov ex1))
