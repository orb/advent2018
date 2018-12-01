(ns advent2018.day1
  (:require [clojure.string :as string]))

(defn read-numbers [text]
  (->> (string/split text #"\n")
       (map string/trim)
       (map #(Integer/parseInt %) )))

(defn part1 [text]
  (let [numbers (read-numbers text)]
    (reduce + numbers)))

(defn part2 [text]
  (loop [numbers (cycle (read-numbers text))
         seen #{0}
         frequency  0]
    (let [number (first numbers)
          new-frequency (+ frequency number)]
      (if (seen new-frequency)
        new-frequency
        (recur (rest numbers)
               (conj seen new-frequency)
               new-frequency)))))

