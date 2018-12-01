(ns advent2018.day1
  (:require [clojure.string :as string]))

(defn read-numbers [text]
  (->> (string/split text #"\n")
       (map string/trim)
       (map #(Integer/parseInt %))))

(defn part1 [text]
  (let [numbers (read-numbers text)]
    (reduce + numbers)))

(defn first-dup
  "returns the first duplicate number in a seq of numbers, if any"
  [numbers]
  (loop [[number & rest-numbers] numbers
         seen #{}]
    (when number
      (if (seen number)
        number
        (recur rest-numbers
               (conj seen number))))))

(defn sum-stream
  "returns a stream of the partial sum of a seq of numbers.
   assumption that the initial sum, 0, should be included"
  [numbers]
  (reductions + 0 numbers))

(defn part2 [text]
  (let [numbers (cycle (read-numbers text))]
    (first-dup (sum-stream numbers))))

