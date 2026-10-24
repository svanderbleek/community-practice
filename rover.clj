(ns rover
  (:require [clojure.string :as str]))

(defn ->plateau [x y]
  {:plateau {:lower 0
             :left  0
             :upper (Integer/parseInt y)
             :right (Integer/parseInt x)}})

(defn s-key [prefix n]
  (keyword (str prefix "-" n)))

(defn ->rover [n x y d]
  {(s-key "rover" n) {:x (Integer/parseInt x)
                      :y (Integer/parseInt y)
                      :d d}})

(defn ->commands [n & commands]
  {(s-key "commands" n) commands})

(defn consume [tokens]
  [(drop 1 (drop-while #(not (= "\n" %)) tokens))
   (take-while #(not (= "\n" %)) tokens)])

(def structure
  [->plateau
   (partial ->rover 1)
   (partial ->commands 1)
   (partial ->rover 2)
   (partial ->commands 2)])

(second (consume (first (consume (re-seq #"\d|[A-Z]|\n" ex1)))))

(defn parse [input]
  (loop [tokens  (re-seq #"\d|[A-Z]|\n" input)
         parsing structure
         state   {}]
    (if (empty? parsing)
      state
      (let [[remain cur]  (consume tokens)
            updated (conj state (apply (first parsing) cur))]
        (recur remain (rest parsing) updated)))))

(def d->xy
  {"N" [ 0  1]
   "E" [ 1  0]
   "S" [ 0 -1]
   "W" [-1  0]})

(defn move [rover]
  (let [[x y] (get d->xy (:d rover))]
    (-> rover
        (update :x + x)
        (update :y + y))))

(def r->d->d
  {"L" {"N" "W"
        "E" "N"
        "S" "E"
        "W" "S"}
   "R" {"N" "E"
        "E" "S"
        "S" "W"
        "W" "N"}})

(defn rotate [r-key rover]
  (assoc rover :d (get-in r->d->d [r-key (:d rover)])))

(def commands->action
  {"M" move
   "L" (partial rotate "L")
   "R" (partial rotate "R")})

(defn in-plateau? [{:keys [left right lower upper]} [x y]]
  (and (<= left x right)
       (<= lower y upper)))

(defn not-blocked? [{:keys [b-x b-y]} [x y]]
  (not (and (= b-x x)
            (= b-y y))))

(defn exec-command [rover command plateau block]
  (let [moved ((get commands->action command) rover)
        pos   ((juxt :x :y) moved)]
    (println pos)
    (if (and (in-plateau? plateau pos)
             (not-blocked? block pos))
      moved
      rover)))

(defn exec-commands [rover commands plateau block]
  (println "moving")
  (loop [remain commands
         moved  rover]
    (if (empty? remain)
      moved
      (recur (rest remain)
             (exec-command moved (first remain) plateau block)))))

(defn b-key [n]
  (case n
    1 (s-key "rover" 2)
    2 (s-key "rover" 1)))

(defn run-rover [state n]
  (let [r-key    (s-key"rover" n)
        rover    (r-key state)
        block    ((b-key n) state)
        plateau  (:plateau state)
        commands ((s-key "commands" n) state)]
    (assoc state r-key (exec-commands rover commands plateau block))))

(defn run [state]
  (run-rover (run-rover state 1) 2))

(defn out-str [rover]
  (str/join " " ((juxt :x :y :d) rover)))

(defn solution [state]
  (str/join "\n" (map out-str ((juxt :rover-1 :rover-2) state))))

(defn solve [input]
  (solution (run (parse input))))

(def ex1
  "5 5
1 2 N
LMLMLMLMM
3 3 E
MMRMMRMRRM")

(solve ex1)
