(ns advent2018.day17
  (:require [instaparse.core :as instaparse]))

;; I made quick refactor of drop-flow to remove some redundant checks
;; that I introduced to handle edge cases/bugs. There's still a lot
;; (slow) work being repeated across iterations. I could save some of
;; that in the loop state to improve performance, or drop-flow could
;; complete an entire line entire line of flow/rest in one
;; iteration. In any case, even with the improvements, it's still
;; taking at least 2 orders of magnitude more time than it should.

;; ----------------------------------------
;; parser

(def grammar
  (instaparse/parser "
    lines = (line <NL>)*
    line = x <SEP> y | y <SEP> x
    x = <'x'> <EQ> range
    y = <'y'> <EQ> range
    range = number | number <DOTS> number
    DOTS = '..'
    EQ = '='
    SEP = ', '
    number=#'[0-9]+'
    NL = '\n'
"))

(def transform {:lines vector
                :line (fn [x y] (merge x y))
                :x (fn [n] {:x n})
                :y (fn [n] {:y n})
                :range vector
                :instructions vector
                :number #(Integer/parseInt %)})

(defn parse [input]
  (instaparse/transform transform (instaparse/parse grammar input)))

;; ----------------------------------------

(def SOURCE \+)
(def CLAY \#)
(def FLOW \|)
(def REST \~)

(defn scan-range [[from to]]
  (if to
    (range from (inc to))
    (range from (inc from))))

(defn make-world [scan]
  (into {}
   (for [xy scan
         x (scan-range (:x xy))
         y (scan-range (:y xy))]
     [[x y] CLAY])))

(defn print-world [world]
  (let [maxy (reduce max (map second (keys world)))
        minx (reduce min (map first (keys world)))
        maxx (reduce max (map first (keys world)))]
    (doseq [y (range (inc maxy))]
      (doseq [x (range minx (inc maxx))]
        (print (if-let [state (world [x y])] state " ")))
      (println "  " y))))

(defn move [[x y] dir]
  (cond
    (= dir :up) [x (dec y)]
    (= dir :down) [x (inc y)]
    (= dir :left) [(dec x) y]
    (= dir :right) [(inc x) y]))


(defn check-wall [world point dir]
  (cond
    (#{CLAY REST} (world point))
    true

    (#{FLOW SOURCE} (world point))
    false

    (#{CLAY REST} (world (move point :down)))
    (check-wall world (move point dir) dir)

    :else
    false))

(defn move-until-drop [world point dir]
  (if (world point)
    nil ;; something there - no drop
    (let [down (move point :down)
          down-thing (world down)]
      (if (or (not down-thing)
              (= FLOW down-thing))
        point
        (recur world (move point dir) dir)))))


(defn move-until-stop [world point dir]
  (loop [at-point point]
    (let [next-point (move at-point dir)
          next-thing (world next-point)]
      (if next-thing
        (when (not= point at-point)
          at-point)
        (recur next-point)))))


(defn drop-flow [world start]
  ;; this is slow and complex. I drop from the top of the world
  ;; one drop at a time, letting each one REST or FLOW
  (let [maxy (reduce max (map second (keys world)))]
    (loop [point start]
      (let [down (move point :down)]
        (if (not (world down))
          ;; nothing below us
          (if (= (second down) maxy)
            [down FLOW]
            (recur down))

          ;; something below us.
          (if (= FLOW (world down))
            ;; if it's flow, we flow
            [point FLOW]

            ;; otherwise move left or right to a drop point
            (if-let [drop  (or (move-until-drop world point :left)
                               (move-until-drop world point :right))]
              (recur drop)

              ;; and if there's no drop point, move left or right to
              ;; an item. the state will bve rest if both sides are walled,
              ;; otherwise it will be a flow
              (let [walled (and (check-wall world point :left)
                                (check-wall world point :right))
                    left   (move-until-stop world point :left)
                    right  (move-until-stop world point :right)]

                [(or left right point)
                 (if walled REST FLOW)]))))))))




(defn flow
  "run the flow simulation and return the frequencies of all the things"
  [input]
  (let [scan (parse input)
        start-world (make-world scan)
        miny (reduce min (map second (keys start-world)))
        source-xy [500 (dec miny)]]
    (loop [world start-world
           n 0]
      (let [[pos tile] (drop-flow world source-xy)]
        (if (= source-xy pos)
          (do
            (def ww world)
            (frequencies (vals world)))
          (recur (assoc world pos tile)
                 (inc n)))))))


(defn part1 [input]
  (reduce + (-> (flow input)
                (select-keys [REST FLOW])
                (vals))))

(defn part2 [input]
  (reduce + (-> (flow input)
                (select-keys [REST])
                (vals))))
