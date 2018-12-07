(ns advent2018.day7-test
  (:require [advent2018.day7 :refer :all]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))


(def sample-input
  "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")

(def my-input (slurp (io/resource "day7-input.txt")))

(deftest test-part1
  (is (= "CABDFE" (part1 sample-input)))
  (is (= "BDHNEGOLQASVWYPXUMZJIKRTFC" (part1 my-input))))

(deftest test-part2
  (is (= 15 (part2 sample-input 2 0)))
  (is (= 1107 (part2 my-input 5 60))))
