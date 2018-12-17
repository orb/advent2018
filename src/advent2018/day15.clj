(ns advent2018.day15
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

;; amazingly ugly here. My first version didn't have a clear exit
;; strategy so I had determine the results from
;; printlns. Ugghh... I've hacked in the final results, but the
;; remaining code here is largely as it was on the first pass. Bad
;; names, bad abstractions and thue ugliest hacked together search you
;; can imagine. Again - uggghhh... I'm not going to bother commenting
;; the code before a refactor

;; I think this was a nice idea for a problem, but it feels a little
;; bit like "I wrote a very specific battle simulator and now I'm
;; going to describe exactly what I wrote and your job is to try and
;; understand the exact details and duplicate it" rather than your
;; typical "here's a fun problem, solve it". Given that I now
;; understand the problem clearly, I dislike it much less than I did
;; the night of the problem, but I still think it was not a well-scoped
;; problem.


(def EMPTY {:type :empty :display \.})
(defn translate-char [c]
  (cond
    (= \. c) {:type :empty :display \.}
    (= \# c) {:type :wall  :display \#}
    (= \G c) {:type :gnome :hp 200 :attack 3 :display \G :id (name (gensym "gnome."))}
    (= \E c) {:type :elf   :hp 200 :attack 3 :display \E :id (name (gensym "elf."))}))

(defn parse [input]
  (vec
   (for [line (string/split input #"\n")]
     (vec
      (map translate-char line)))))

(defn tile-at [cave [x y]]
  (get-in cave [y x]))

(defn set-tile-at-point [cave [x y] tile]
  (assoc-in cave [y x] tile))

(defn print-cave [cave]
  (doseq  [y (range  0 (count cave))]
    (let [notes
          (doall
           (for [x (range 0 (count (cave y)))]
             (let [tile (tile-at cave [x y])]
               (print (:display tile))
               (if (:id tile) (format " %s-[%s]" (:id tile) (:hp tile))))))]
      (println (string/join notes)))))

(defn sum-hp [cave unit-type]
  (reduce + 0
          (for  [y (range  0 (count cave))
                 x (range 0 (count (cave y)))
                 :let [tile (tile-at cave [x y])]
                 :when (= unit-type (:type tile))]
            (:hp tile))))

(defn count-type [cave unit-type]
  (count
   (for  [y (range  0 (count cave))
          x (range 0 (count (cave y)))
          :let [tile (tile-at cave [x y])]
          :when (= unit-type (:type tile))]
     tile)))

(defn order-units [cave]
  (for [y (range 0 (count cave))
        x (range 0 (count (cave y)))
        :let [tile (tile-at cave [x y])]
        :when (#{:elf :gnome} (:type tile))]
    (:id tile)))


(defn find-id [cave target]
  (first
   (for [y (range 0 (count cave))
         x (range 0 (count (cave y)))
         :when (= target (:id (tile-at cave [x y])))]
     [x y])))


(defn move [[x y] dir]
  (cond
    (= dir :up) [x (dec y)]
    (= dir :down) [x (inc y)]
    (= dir :left) [(dec x) y]
    (= dir :right) [(inc x) y]))

(defn surrounding-points [cave point]
  [(move point :up)
   (move point :left)
   (move point :right)
   (move point :down)])

(defn surrounding-tiles [cave point]
  [(tile-at cave (move point :up))
   (tile-at cave (move point :left))
   (tile-at cave (move point :right))
   (tile-at cave (move point :down))])


(defn empty-tiles [tiles]
  (filter #(= :empty (:type %)) tiles))

(defn empty-points [cave points]
  (filter #(= :empty (:type (tile-at cave %))) points))

(defn can-attack [cave unit target]
  (and (:id target)
       (not= (:type unit)
             (:type target))))

(defn find-targets [cave unit]
  (let [attacker (tile-at cave (find-id cave unit))]
    (for [y (range 0 (count cave))
          x (range 0 (count (cave y)))
          :let [target (tile-at cave [x y])]
          :when (can-attack cave attacker target)]
      (:id target))))

(defn shortest-path [cave start-point end-points]
  (let [goals (set (mapcat #(empty-points cave (surrounding-points cave %)) end-points))
        follow-path (fn follow-path [visit point]
                      (if-let [parent (visit point)]
                        (cons point (follow-path (dissoc visit point) parent))
                        (list point)))

        sort-frontier (fn [frontier]
                        (sort-by (fn [[[x y] _]]
                                   [y x])
                                 frontier))
        frontier-points (fn [visited point]
                          (let [res (vec
                                     (for [neighbor (empty-points cave (surrounding-points cave point))
                                           :when (not (visited neighbor))]
                                       [neighbor point]))]
                            (vec res)))
        merge-frontiers (fn [frontiers]
                          (reduce (fn [frontier potential]
                                    (let [in-frontier (set (map first frontier))]
                                      (if (in-frontier (first potential))
                                        frontier
                                        (conj frontier potential ))))
                                  []
                                  frontiers))]
    (loop [frontier {start-point nil}
           visited {}
           n 0 ]

      (Thread/sleep 1)
      (cond
        (empty? goals)
        nil

        (some goals (keys visited))
        (let [arrived (filter goals (keys visited))
              best (first (sort-by (fn [[x y]] [y x]) arrived))]
          (let [path (follow-path visited best)]
            (last (butlast path))))

        (empty? frontier)
        nil

        :else
        (let [points (map first frontier)
              npoints (mapcat #(frontier-points visited %) points)
              merged (merge-frontiers npoints)
              sorted (sort-frontier merged)
              next-frontier sorted
              next-visited (reduce (fn [v [from to]]
                                     (assoc v from to))
                                   visited
                                   next-frontier)]
          (recur next-frontier
                 next-visited
                 (inc n)))))))

(defn try-attack [cave unit]
  (let [point (find-id cave unit)
        attacker (tile-at cave point)
        possible-victims (sort-by :hp (for [neighbor (surrounding-tiles cave point)
                                            :when (can-attack cave attacker neighbor)]
                                        neighbor))]
    (if-let [victim (first possible-victims)]
      (let [victim-point (find-id cave (:id victim))
            victim-damaged (update victim :hp #(- % (:attack attacker)))]
        ;;(println "  ATTACK! " unit  "->" victim-damaged)
       (if (<= (:hp victim-damaged) 0)
          (let [remove-dead (set-tile-at-point cave victim-point EMPTY)]
            #_(println "  RIP" (:id victim) )
            #_(println "  ZZZZZ HP E"
                     (sum-hp remove-dead :elf)
                     (sum-hp remove-dead :gnome))
            remove-dead)
          (set-tile-at-point cave victim-point victim-damaged)))
      cave)))

(defn take-turn [run-state unit]
  (if (:done run-state)
    run-state
    (let [cave (:cave run-state)]
      (if-let [point (find-id cave unit)]
        (let [ ;;_ (println "TURN" unit "at" point)
              attacker (tile-at cave point)
              neighbors (surrounding-tiles cave point)
              attack? (some true? (map #(can-attack cave attacker %) neighbors))]
          (if attack?
            (assoc run-state :cave (try-attack cave unit)) ;; skip move
            (if-let [targets (seq (find-targets cave unit))]
              (let [target-points (map #(find-id cave %) targets)]
                (if-let [move-to (shortest-path cave point target-points)]
                  (assoc run-state
                         :cave (-> cave
                                   (set-tile-at-point move-to (tile-at cave point))
                                   (set-tile-at-point point EMPTY)
                                   (try-attack unit)))
                  run-state))
              (assoc run-state :done true)))) ;; no targets - done!!!!
        run-state)))) ;; unit is gone

(defn part1 [input]
  (loop [run-state {:n 0
                    :done false
                    :cave (parse input)}]

    #_(println "!----------------------------------------" (:n run-state))
    #_(print-cave (:cave run-state))

    (let [next-state (reduce take-turn run-state (order-units (:cave run-state)))]
      (if (:done next-state)
        (* (:n run-state)
           (max (sum-hp (:cave next-state) :elf)
                (sum-hp (:cave next-state) :gnome)))
        (recur (-> next-state
                   (update :n inc)))))))

;; ----------------------------------------

(defn set-elf-attack [cave attack]
  (reduce (fn [cave xy]
            (let [elf (tile-at cave xy)]
              (set-tile-at-point cave
                                 xy
                                 (assoc elf :attack attack))))
          cave
          (for [y (range 0 (count cave))
                x (range 0 (count (cave y)))
                :let [tile (tile-at cave [x y])]
                :when (= :elf (:type tile))]
            [x y])))

(defn part2 [input]
  (let [start-cave (parse input)
        n-elves (count-type start-cave :elf)]
    (loop [attack 3]
      (if-let [score (loop [run-state {:n 0
                                       :done false
                                       :cave (set-elf-attack start-cave attack)}]

                       (let [next-state (reduce take-turn run-state (order-units (:cave run-state)))]
                         (if (:done next-state)
                           (when (= n-elves
                                    (count-type (:cave next-state) :elf))
                             (* (:n next-state)
                                (sum-hp (:cave next-state) :elf)))

                           (recur (update next-state :n inc)))))]
        score
        (recur (inc attack))))))
