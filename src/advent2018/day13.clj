(ns advent2018.day13
  (:require [clojure.string :as string]))

;; This problem was quite refreshing, after yesterday's subpar problem. I finished in about the same
;; amount of time, but I feel a lot better about today.
;;
;; my solution here is rather verbose. I refactored only slightly, leaving in a lot of my initial code,
;; some of which turned out to be not needed for the final solution at all. Some notes:
;;
;; - I normalized the track, removing all the carts from the map.
;; - I misunderstood the "no memory of intersections" to mean that all the carts had no memory
;;   and if cart 1 turned left and then cart 2 would go straight. That's why cart map has the next choice
;;   bolted on like it is.
;; - I feel like I could have mathed the navigation and turns a bit better. In the interest of speed
;;   and clarity, I opted for lots of lookup tables and conds.
;; - I'd prefer that each loop iteration moved all the carts, but as with earlier problems,
;;   it just seems more straightforward initially to update 1 cart per iteration. I really need
;;   to stop and think about how I should be approaching these problems, because I really am not
;;   happy with that.
;; - I normally don't loop/recur with a large state object, but iniially I thought I would need
;;   maxx, maxy and maybe more, I started with that.


(def track-only
  {\^ \|
   \v \|
   \> \-
   \< \-
   \| \|
   \- \-
   \\ \\
   \/ \/
   \+ \+})

(def cart-dir
  {\^ :up
   \v :down
   \> :right
   \< :left})

(def dir-delta
  {:left [-1 0]
   :right [1 0]
   :down [0 1]
   :up [0 -1]})


(defn parse [input]
  (let [lines (string/split-lines input)]
    (let [maxy (count lines)
          maxx (apply max (map count lines))]
      (let [track
            (for [y (range maxy)
                  x (range maxx)
                  :let [tile (get-in lines [y x])]
                  :when (track-only tile)]
              [[x y] (track-only tile)])

            carts
            (for [y (range maxy)
                  x (range maxx)
                  :let [tile (get-in lines [y x])]
                  :when (cart-dir tile)]
              [[x y] [(cart-dir tile) :left]])]
        {:maxx maxx
         :maxy maxy
         :carts (into {} carts)
         :track (into {} track)}))))

(defn sort-carts
  "retturn the co-ordinates of the carts, in the order we should move them"
  [carts-map]
  (sort (keys carts-map)))

(defn continue
  "continue in a given direction, given the tile we are on
   returns the actual direction we move"
  [dir tile]
  (cond
    (= dir :left)
    (cond
      (= \- tile) :left
      (= \\ tile) :up
      (= \/ tile) :down)
    (= dir :right)
    (cond
      (= \- tile) :right
      (= \\ tile) :down
      (= \/ tile) :up)
    (= dir :up)
    (cond
      (= \| tile) :up
      (= \\ tile) :left
      (= \/ tile) :right)
    (= dir :down)
    (cond
      (= \| tile) :down
      (= \\ tile) :right
      (= \/ tile) :left)))

(defn intersection-dir
  "traverse an intersection, given our current direction and our current turn preference,
   returns the actual directtion to mbove"
  [current-dir choice]
  (cond
    (= :left choice)
    (cond
      (= current-dir :left)  :down
      (= current-dir :right) :up
      (= current-dir :up)    :left
      (= current-dir :down)  :right)
    (= :right choice)
    (cond
      (= current-dir :left)  :up
      (= current-dir :right) :down
      (= current-dir :up)    :right
      (= current-dir :down)  :left)
    (= :straight choice)
    current-dir))

(defn next-choice [choice]
  (cond
    (= :left choice)      :straight
    (= :straight choice) :right
    (= :right choice)    :left))

(defn intersection? [tile]
  (= \+ tile))

(defn crash?
  "will there be crash if a cart moves to the given co-ordinates"
  [state move-to]
  ((:carts state) move-to))

(defn part1 [input]
  ;; I refactored slightly for part 2, but I've left part1 in the form
  ;; it was when I finished it The primary difference is the
  ;; duplication of a lot of code between the intersection/
  ;; non-interection cases
  (let [state (parse input)]
    (loop [carts []
           tick 0
           state state]
      (if (seq carts)
        (let [cart (first carts)
              [dir choice] ((:carts state) cart)
              tile ((:track state) cart)]
          (if (= \+ tile)
            (let [move-dir (intersection-dir dir choice)
                  delta (dir-delta move-dir)
                  move-to (into [] (map + cart delta))]
              #_(println " I" [cart dir] choice move-dir move-to)
              (if (crash? state move-to)
                move-to
                (recur (rest carts)
                       tick
                       (update state :carts
                               (fn [carts]
                                 (-> carts
                                     (dissoc cart)
                                     (assoc move-to [move-dir (next-choice choice)])))))))
            (let [move-dir (continue dir tile)
                  delta (dir-delta move-dir)
                  move-to (into [] (map + cart delta))]
              #_(println " M" [cart dir] tile "->" move-to)
              (if (crash? state move-to)
                move-to
                (recur (rest carts)
                       tick
                       (update state :carts
                               (fn [carts]
                                 (-> carts
                                     (dissoc cart)
                                     (assoc move-to [move-dir choice])))))))))
        (do
          #_(println "T" tick (sort-carts (:carts state)))
          (recur (sort-carts (:carts state))
                 (inc tick)
                 state))))))


(defn part2 [input]
  (let [state (parse input)]
    (loop [carts [] ;; the list of carts to be moved
           state state]
      (if-let [cart (first carts)]
        (let [[dir choice] ((:carts state) cart)
              tile ((:track state) cart)
              move-dir (if (intersection? tile)
                         (intersection-dir dir choice)
                         (continue dir tile))
              move-to (into [] (map + cart (dir-delta move-dir)))]
          (if (crash? state move-to)
            (recur (rest carts)
                   (update state :carts
                           dissoc cart move-to))
            (recur (rest carts)
                   (update state :carts
                           (fn [carts]
                             (-> carts
                                 (dissoc cart)
                                 (assoc move-to [move-dir (if (intersection? tile)
                                                            (next-choice choice)
                                                            choice)])))))))
        (if (= 1 (count (:carts state)))
          (first (keys (:carts state))) ;; exit condition, one car left
          (recur (sort-carts (:carts state)) ;; next tick, refill the work queue and move all the carts
                 state))))))
