(ns advent2018.day20-inputs
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def my-input (string/trim (slurp (io/resource "day20-input.txt"))))
