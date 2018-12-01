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


(deftest test-sums
  (is (= [0 1 3 6]
         (sum-stream [1 2 3])))
  (is (= (range 10)
         (take 10 (sum-stream (repeatedly (constantly 1))))))
  (is (= (take 7 (cycle [0 5]))
         (take 7 (sum-stream (cycle [5 -5]))))))

(deftest test-dup
  (is (nil? (first-dup [8 6 7 5 3 0 9])))
  (is (= 4 (first-dup [2 4 6 8 10 5 1 4]))))

(deftest test-part2
  (is (= 0 (part2 "+1\n -1")))
  (is (= 10 (part2 "+3\n +3\n +4\n -2\n -4")))
  (is (= 5 (part2 "-6\n +3\n +8\n +5\n -6")))
  (is (= 14 (part2 "+7\n +7\n -2\n -7\n -4")))
  (is (= 78724 (part2 my-input))))



