(ns advent2018.day25
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [instaparse.core :as instaparse]
            [clojure.set :as set]))

;; I was really wondering what part2 would be... Jokes on me!  My
;; solution here built a graph of points with edges betwen the close
;; points. I picked a point and followed the graph to get everything
;; reachable in that group. Repeat until everything was in a group

(def my-input (slurp (io/resource "day25-input.txt")))


(def grammar
  (instaparse/parser "
    lines = (line <NL>)*
    line = number (<','> number)*
    number = #'-?[0-9]+'
    NL = '\n'
"))

(def transform {:lines vector
                :line vector
                :number #(Integer/parseInt %)})

(defn parse [input]
  (instaparse/transform transform (instaparse/parse grammar input)))


(defn distance [p1 p2]
  (reduce + (map #(Math/abs ^int (- %1 %2)) p1 p2)))

(defn find-reachable
  "given a graph, return a set of nodes reachable from a given node"
  [graph node]
  (loop [frontier #{node}
         visited #{}]
    (if (empty? frontier)
      visited
      (let [visit (first frontier)
            neighbors (for [[from to] graph
                            :when (and (= from visit)
                                       (not (visited to)))]
                        to)]
        (recur (-> frontier
                   (disj visit)
                   (into neighbors))
               (conj visited visit))))))

(defn build-graph [points]
  (reduce merge
          (for [i1 (range (count points))
                i2 (range (inc i1) (count points))
                :let [p1 (points i1)
                      p2 (points i2)]
                :when (<= (distance p1 p2) 3)]
            [[p1 p2]
             [p2 p1]])))

(defn part1 [input]
  (let [points (parse input)
        graph (build-graph points)]
    (loop [groups 0
           unvisited (set points)]
      ;; pick an unvisited point
      ;; find all the points reachable from that point
      ;; remove those points and bump the group counter
      (if (empty? unvisited)
        groups
        (recur (inc groups)
               (set/difference unvisited (find-reachable graph (first unvisited))))))))
