(ns advent2018.day13-test
  (:require [advent2018.day13 :refer :all]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(def sample-input (slurp (io/resource "day13-sample.txt")))
(def my-input (slurp (io/resource "day13-input.txt")))


(deftest test-part1
  (is (= [7 3] (part1 sample-input)))
  (is (= [80 100] (part1 my-input))))

(deftest test-part2
  (is (= [16 99] (part2 my-input))))

