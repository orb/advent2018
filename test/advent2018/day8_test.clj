(ns advent2018.day8-test
  (:require [advent2018.day8 :refer :all]
            [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def sample-input "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")
(def my-input (string/trim (slurp (io/resource "day8-input.txt"))))

(deftest test-part1
  (is (= 138 (part1 sample-input)))
  (is (= 37905 (part1 my-input))))

(deftest test-part2
  (is (= 66 (part2 sample-input)))
  (is (= 33891 (part2 my-input))))

