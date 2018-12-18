(ns advent2018.day17-test
  (:require [advent2018.day17 :refer :all]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(def sample-input "x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504
")

(def my-input (slurp (io/resource "day17-input.txt")))

(deftest test-part1
  (is (= 57 (part1 sample-input)))
  (is (= 31667 (part1 sample-input))))

(deftest test-part2
  (is (= 29 (part2 sample-input)))
  (is (= 25018 (part2 sample-input))))

