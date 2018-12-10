(ns advent2018.day9-test
  (:require [advent2018.day9 :refer :all]
            [clojure.test :refer :all]))

;; my input
;; 479 players; last marble is worth 71035 points

(deftest test-part1
  (is (= 32 (part1 9 25)))
  (is (= 8317 (part1 10 1618)))
  (is (= 37305 (part1 30 5807)))
  (is (= 367634 (part1 479 71305))))

(deftest test-part2
  (is (= 3020072891 (part2 479 7103500))))
