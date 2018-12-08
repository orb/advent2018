(ns advent2018.day8
  (:require [instaparse.core :as instaparse]
            [clojure.string :as string]))

;;----------------------------------------
(defn parse [input]
  (map #(Integer/parseInt %) (string/split input #" ")))

(defn add [numbers]
  (reduce + 0 numbers))

;; ----------------------------------------

(defn read-node [numbers]
  ;; this is a real pain to code functionally
  ;; everything has to return a result and the new stream of numbers
  ;;
  ;; I decided to re-use the number numbers repeatedly because, as
  ;; ugly as it is to shadow a name over and over, I think it would ne
  ;; worse to have to invent a new name for each one...
  ;;
  ;; there has to be a cleaner way...
  (let [[n-children n-metadata & numbers] numbers]
    (let [[children numbers]
          (loop [n 0
                 children []
                 numbers numbers]
            (if (= n n-children)
              [children numbers]
              (let [[child numbers] (read-node numbers)]
                (recur (inc n)
                       (conj children child)
                       numbers))))]
      (let [metadata (take n-metadata numbers)]
        [{:children children :metadata metadata}
         (drop n-metadata numbers)]))))

(defn sum-meta [node]
  (if node
    (+ (add (:metadata node))
       (add (map sum-meta (:children node))))
    0))

(defn part1 [input]
  (let [[node _] (read-node (parse input))]
    (sum-meta node)))

;; ----------------------------------------


(defn sum2 [node]
  (add
   (if-let [children (seq (:children node))]
     (for [index (:metadata node)
           :let [child (nth children (dec index) nil)]
           :when child]
       (sum2 child))
     (:metadata node))))

(defn part2 [input]
  (let [[node _] (read-node (parse input))]
    (sum2 node)))
