(ns advent2018.day6-test
  (:require [advent2018.day6 :refer :all]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(def sample-input1
  "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9")

(def my-input (slurp (io/resource "day6-input.txt")))


(deftest test-part1
  (is (= 17 (part1 sample-input1)))
  (is (= 4215 (part1 my-input))))

(deftest test-part1
  (is (= 16 (part2 sample-input1 32)))
  (is (= 40376 (part2 my-input 10000))))
