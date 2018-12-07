(ns advent2018.day6
  (:require [instaparse.core :as instaparse]))

;; I didn't have any time to clean this up, comment it or event test it beyond
;; just gettting it going.

(def grammar (instaparse/parser "
    lines = line (<NL> line)* <NL>?
    line = number <COMMA> <SP>  number
    number = #'[0-9]+'
    NL = '\n'
    SP = ' '
    COMMA = ','
"))

(def transform {:lines vector
                :line vector
                :number #(Integer/parseInt %)})

(defn parse [input]
  (instaparse/transform transform (instaparse/parse grammar input)))

(defn manhatten [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

;; ----------------------------------------
(defn xrange [points]
  (let [xs (map first points)]
    (range (apply min xs) (inc (apply max xs)))))

(defn yrange [points]
  (let [ys (map second points)]
    (range (apply min ys) (inc (apply max ys)))))


;; ----------------------------------------
(defn find-closest [target-point points]
  (let [distances (zipmap points
                          (map (partial manhatten target-point) points))
        sorted (sort-by val distances)
        p1 (first sorted)
        p2 (second sorted)]
    (if (< (second p1) (second p2))
      (first p1)
      nil)))


(defn make-closest-map [points]
  (println "making map..." (count points))
  (for [x (xrange points)
        y (yrange points)]
    (let [closest (find-closest [x y] points)]
      {:point [x y]
       :closest closest})))

(defn remove-deadspace [point-map]
  (filter #(:closest %) point-map))

(defn remove-edgepoints [point-map points]
  (println "removing edgepoints" (count point-map))
  (let [xs (xrange points)
        ys (yrange points)
        minx (first xs)
        maxx (last xs)
        miny (first ys)
        maxy (last ys)

        edgepoints (set (for [{:keys  [point closest]} point-map
                              :let [[x y] point]
                              :when (or (= minx x)
                                        (= maxx x)
                                        (= miny y)
                                        (= maxy y))]
                          closest))

        filtered-map (remove #(edgepoints (:closest %)) point-map)
        freqs (frequencies (map :closest filtered-map))]

    (apply max (vals freqs))))


(defn part1 [input]
  (let [points (parse input)]
    (-> (make-closest-map points)
        (remove-deadspace)
        (remove-edgepoints points))))

;; ----------------------------------------

(defn make-distances [points]
  (for [x (xrange points)
        y (yrange points)]
    {:point [x y]
     :total (reduce + (map (partial manhatten [x y]) points))}))

(defn part2 [input limit]
  (let [points (parse input)]
    (->> (make-distances points)
         (filter #(< (:total %) limit))
         (count))))

