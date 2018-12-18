(ns advent2018.day18-inputs
  (:require [clojure.java.io :as io]))

(def sample-input ".#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.")

(def my-input (slurp (io/resource "day18-input.txt")))
