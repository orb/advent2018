(ns advent2018.day4-test
  (:require [advent2018.day4 :refer :all]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(def sample-input
  "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up")

(def my-input (slurp (io/resource "day4-input.txt")))

(deftest test-part1
  (is (= 240 (part1 sample-input)))
  (is (= 20859 (part1 my-input))))

(deftest test-part2
  (is (= 4455 (part2 sample-input)))
  (is (= 76576 (part2 my-input))))
