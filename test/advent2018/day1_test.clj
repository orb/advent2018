(ns advent2018.day1-test
  (:require [advent2018.day1 :refer :all]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(def my-input (slurp (io/resource "day1-input.txt")))

(deftest test-read
  (is (= [-1 1 2] (read-numbers " -1  \n1\n+2   "))))

(deftest test-part1
  (is (= 3 (part1 "+1\n +1\n +1")))
  (is (= 553 (part1 my-input))))

(deftest test-part2
  (is (= 0 (part2 "+1\n -1")))
  (is (= 10 (part2 "+3\n +3\n +4\n -2\n -4")))
  (is (= 5 (part2 "-6\n +3\n +8\n +5\n -6")))
  (is (= 14 (part2 "+7\n +7\n -2\n -7\n -4")))
  (is (= 78724 (part2 my-input))))

