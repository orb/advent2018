(ns advent2018.day14-test
  (:require [advent2018.day14 :refer :all]
            [clojure.test :refer :all]))

(deftest test-part1
  (is (= "0124515891" (part1 5)))
  (is (= "9251071085" (part1 18)))
  (is (= "5941429882" (part1 2018)))
  (is (= "5101271252" (part1 30121))))

(deftest test-part2
  (is (= 9        (part2 "51589")))
  (is (= 5        (part2 "01245")))
  (is (= 2018     (part2 "59414")))
  (is (= 20287556 (part2 "030121"))))
