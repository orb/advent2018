(ns advent2018.day18
  (:require [advent2018.day18-inputs :as inputs]
            [clojure.string :as string]))

;; I've changed up my basic pattern here, moving inputs to their own ns and removing
;; the :refer :all from tests. I think this will make my repl experience better. Let's see
;; if I stick with it or go back to my old pattern

;; Notes:
;; - Again, I've done the world map as hash map from [x y] -> tile type. This
;; is obviously not as efficient, but it makes bounds checking a breeze

(def OPEN \.)
(def TREE \|)
(def LUMBERYARD \#)

;; ----------------------------------------

(defn parse [input]
  ;; parse goes straight to the full state here
  ;; and not just the map.
  (let [lines (vec (map vec (string/split input #"\n")))
        max-y (count lines)
        max-x (count (first lines))
        map (into {}
                  (for [y (range max-y)
                        x (range max-x)]
                    [[x y] (get-in lines [y x])]))]
    {:size [max-x max-y]
     :map map}))

(defn print-state
  "print out the state"
  [state]
  ;; this is the only time I needed the :size, and
  ;; the only case where the hashmap world representation
  ;; wasn't sufficient. And, interestingly, I actually
  ;; used print-state in the cycle check.
  (doseq [y (range (get-in state [:size 1]))]
    (doseq [x (range (get-in state [:size 0]))]
      (print (get-in state [:map [x y]])))
    (println)))



;; ----------------------------------------

(defn neighbors
  "return the neighoring points, if they exist on the map"
  [state [x y]]
  (filter (:map state)
          [[(dec x) (dec y)]
           [(dec x) y]
           [(dec x) (inc y)]
           [x (dec y)]
           [x (inc y)]
           [(inc x) (dec y)]
           [(inc x) y]
           [(inc x) (inc y)]]))


(defn rules
  "the rules to compute the next state for a tile based on it's neighbors"
  [me neighbors]
  (let [counts (frequencies neighbors)]
    (cond
     (= OPEN me)
     (if (>= (counts TREE 0) 3)
       TREE
       OPEN)

     (= TREE me)
     (if (>= (counts LUMBERYARD 0) 3)
       LUMBERYARD
       TREE)

     (= LUMBERYARD me)
     (if (and (>= (counts LUMBERYARD 0) 1)
              (>= (counts TREE 0) 1))
       LUMBERYARD
       OPEN))))

(defn step
  "take one time step and return tthe new statte"
  [state]
  (update state
          :map
          (fn [world]
            (into {}
                  (for [pos (keys world)
                        :let [tile           (get-in world [pos])
                              neighbor-tiles (map world (neighbors state pos))
                              new-tile       (rules tile neighbor-tiles)]]
                    [pos new-tile])))))


(defn score
  "compute the score of a state"
  [state]
  (let [counts (frequencies (vals (:map state)))]
    (* (counts TREE)
       (counts LUMBERYARD))))

(defn part1 [input]
  (let [final-state  (nth (iterate step (parse input)) 10)]
    (score final-state)))


(defn part2 [input]
  (let [max-n 1000000000]
   (loop [n 0
          state (parse input)
          seen {}]
     (let [state-str (with-out-str (print-state state))]
       (if-let [seen-at (seen state-str)]
         ;; state previously seen
         (let [cycle-size (- n seen-at)
               jump-cycles (quot (- max-n n) cycle-size)
               jump-to (+ n (* cycle-size jump-cycles))]
           ;; calculate the cycle distance and move forward as many cycles as we can
           (recur jump-to
                  state
                  {})) ;; clear out the seen cycles so we don't try this again

         ;; no cycle, keep going
         (if (= n max-n)
           ;; done
           (score state)
           ;; not there yet,
           (recur (inc n)
                  (step state)
                  (assoc seen state-str n))))))))
