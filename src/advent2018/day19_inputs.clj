(ns advent2018.day19-inputs
  (:require [clojure.java.io :as io]))

(def sample-input
  "#ip 0
seti 5 0 1
seti 6 0 2
addi 0 1 0
addr 1 2 3
setr 1 0 0
seti 8 0 4
seti 9 0 5
")

(def my-input
  (slurp (io/resource "day19-input.txt")))
