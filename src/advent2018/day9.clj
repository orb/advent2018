(ns advent2018.day9)

;; I don't see any way to solve the larger problems efficiently in a
;; purely functional way, so I quickly experiemented with mutable data
;; strtuctures. I've retained the main three versions


;; ----------------------------------------
;; my first version used vectors. The code is not beautiful. The insert
;; and remvoe operations should ber factored out. player/marble counter
;; can likely be combined into one structure, etc...
;; side note, this version replaces a first implementation using take/drop
;; instead of subvec, but really with into/concat there's are performance limits here
;; and I can't see any other way to use the functional structures to get
;; the desired performance.

(defn next-pos [n-marbles current-pos]
  (inc (mod (inc current-pos) n-marbles)))

(defn play-vector [n-players last-marble]
  (loop [player 0
         marble 1
         scores (into [] (take n-players (repeatedly (constantly 0))))
         current-pos 0
         marbles [0]]
    (if (> marble last-marble)
      (apply max scores)
      (if (= 0 (mod marble 23))
        (let [remove-at (mod (- current-pos 6) (count marbles))
              removed-marble (marbles (dec remove-at))]
          (recur (mod (inc player) n-players)
                 (inc marble)
                 (update scores player + marble removed-marble)
                 (dec remove-at)
                 (into []
                       (concat (subvec marbles 0 (dec remove-at))
                               (subvec marbles remove-at)))))
        (let [insert-at (next-pos (count marbles) current-pos)]
          (recur (mod (inc player) n-players)
                 (inc marble)
                 scores
                 insert-at
                 (if (= insert-at (count marbles))
                   (conj marbles marble)
                   (into []
                         (concat (subvec marbles 0 insert-at)
                                 [marble]
                                 (subvec marbles insert-at))))))))))

;; ----------------------------------------
;; failing to see a way forward, I payed a visit to my college buddy
;; friend java.util.ArrayList. This was a huge speedup, but it didn't
;; solve the fundamental performance problem. This is basically a direct
;; translation of the vector code.

(defn play-arraylist [n-players last-marble]
  (let [marbles (doto (java.util.ArrayList. last-marble)
                       (.add 0))]
    (loop [player 0
           marble 1
           scores (into [] (take n-players (repeatedly (constantly 0))))
           current-pos 0]
      (if (> marble last-marble)
        (apply max scores)
        (if (= 0 (mod marble 23))
          (let [remove-at (mod (- current-pos 6) (.size marbles))
                removed-marble (.get marbles (dec remove-at))]
            (.remove marbles (int (dec remove-at)))
            (recur (mod (inc player) n-players)
                   (inc marble)
                   (update scores player + marble removed-marble)
                   (dec remove-at)))
          (let [insert-at (next-pos (.size marbles) current-pos)]
            (if (= insert-at (.size marbles))
              (.add marbles marble)
              (.add marbles insert-at marble))
            (recur (mod (inc player) n-players)
                   (inc marble)
                   scores
                   insert-at)))))))


;; ----------------------------------------

;; ultimately, I realize the best solution would a doubly linked list, and
;; a circular list would be exremely convenient. So, I hacked together
;; a crude implementation using volatiles, which I somehow only now discovered
;; after looking at other people's solutions for previous problems.
;;
;; misc notes:
;; volatiles are not thread safe.
;; this implementation is doesn't handle an empty circular list.
;; A reference to a detached node may not make sense.
;; The repl will barf trying try println the list
;; no ease of life functions provided (like count) are provided
;; I don't track the head node or treat any node as special
;; test cases? Surely you jest!

(defn ll-root
  "construct a root node"
  [n]
  (let [node (volatile! {:n n})]
    (vswap! node
            assoc :next node :prev node)
    node))

(defn ll++
  "return the next node"
  [node]
  (:next @node))

(defn ll-- [node]
  "return the previous node"
  (:prev @node))

(defn ll-insert-before [node n]
  (let [nprev (:prev @node)
        new-node (volatile! {:n n
                             :prev nprev
                             :next node})]
    (vswap! nprev assoc :next new-node)
    (vswap! node assoc :prev new-node)
    new-node))

(defn ll-val
  "get the node value"
  [node]
  (:n @node))

(defn ll-remove [node]
  (let [prev (:prev @node)
        next (:next @node)]
    (vswap! prev assoc :next next)
    (vswap! next assoc :prev prev)
    next))

;; ----------------------------------------
(defn play-circular [n-players last-marble]
  (loop [player 0
         marble 1
         scores (into [] (take n-players (repeatedly (constantly 0))))
         marbles (ll-root 0)]
    (if (> marble last-marble)
      (apply max scores)
      (if (= 0 (mod marble 23))
        (let [new-pos (first (drop 7 (iterate ll-- marbles )))
              removed-marble (ll-val new-pos)]
          (recur (mod (inc player) n-players)
                 (inc marble)
                 (update scores player + marble removed-marble)
                 (ll-remove new-pos)))
        (recur (mod (inc player) n-players)
               (inc marble)
               scores
               (ll-insert-before (ll++ (ll++ marbles))
                                 marble))))))

;; ----------------------------------------

(def part1 play-circular)
(def part2 play-circular)
