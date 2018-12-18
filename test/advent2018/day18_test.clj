(ns advent2018.day18-test
  (:require [advent2018.day18 :as day18]
            [advent2018.day18-inputs :as inputs]
            [clojure.test :as t]))

(t/deftest test-part1
  (t/is (= 1147 (day18/part1 inputs/sample-input)))
  (t/is (= 506385 (day18/part1 inputs/my-input))))

(t/deftest test-part2
  (t/is (= 215404 (day18/part2 inputs/my-input))))
