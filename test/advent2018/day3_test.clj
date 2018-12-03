(ns advent2018.day3-test
  (:require [advent2018.day3 :refer :all]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(def sample-input
  "#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2")

(def my-input (slurp (io/resource "day3-input.txt")))

(deftest test-parse
  (is (= 3 (count (parse sample-input)))))

(deftest test-part1
  (is (= 4 (part1 sample-input))
      (= 101469 (part1 my-input))))

(deftest test-part2
  (is (= 3 (part2 sample-input)))
  (is (= 1067 (part2 my-input))))
