(ns advent2018.day24
  (:require [instaparse.core :as instaparse]
            [clojure.java.io :as io]))

;; I got to day 24 late (day 25 has already started and, I assume the leaderboard filled)
;; Rather than fiddle with this any more, I'm going to call it down. There's nothing
;; particularly exciting. The parsing challenge was fun, and although the battle simulator
;; is a mess, I enjoyed trying a few new things. Hope to clean this up some day

(def sample-input (slurp (io/resource "day24-sample.txt")))
(def my-input (slurp (io/resource "day24-input.txt")))

(def grammar
  (instaparse/parser "
    all = <header> <NL> units <NL> <header> <NL> units <NL>*
    header = #'[a-zA-Z ]+:'
    units = (unit <NL>) *
    unit = number <' units each with '> number <' hit points'> resists
           <' with an attack that does '> number <' '> damage-type <' damage'>
           <' at initiative '> number
    resists = <' ('>  (resist <'; '>?)*  <')'> | Epsilon
    resist = resist-type <' to '> damages
    number = #'[0-9]+'
    damages = (<', '>? damage-type)+
    resist-type = 'weak' | 'immune'
    damage-type = 'fire' | 'slashing' | 'radiation' | 'bludgeoning' | 'cold'
    NL = '\n'
    MORE = #'[^\n]+'
"))


(def transform {:all (fn [immune infection]
                       (merge (zipmap (map #(keyword (str "m" %1))
                                           (rest (range)))
                                      (map #(assoc % :type :immune) immune))
                              (zipmap (map #(keyword (str "n" %1))
                                           (rest (range)))
                                      (map #(assoc % :type :infection) infection)))
)
                :units vector
                :unit (fn [n-units hp resists attack attack-type initiative]
                        {:n n-units
                         :hp hp
                         :resists resists
                         :attack attack
                         :attack-type attack-type
                         :initiative initiative})
                :resists merge
                :resist hash-map
                :damages hash-set
                :resist-type keyword
                :damage-type keyword
                :number #(Integer/parseInt %)})

(defn parse [input]
  (instaparse/transform transform (instaparse/parse grammar input)))



;; ----------------------------------------

(defn effective-power [unit]
  (* (:n unit)
     (:attack unit)))

(defn damage-multiplier [attacker defender]
  (let [attack-type (:attack-type attacker)]
    (cond
      (get-in defender [:resists :weak attack-type])
      2

      (get-in defender [:resists :immune attack-type])
      0

      :else
      1)))

(defn damage-delt [attacker defender]
  (* (damage-multiplier attacker defender)
     (:attack attacker)
     (:n attacker)))


(defn select-targets [armies]
  (loop [attacker-ids (reverse
                       (sort-by (fn [unit-id]
                                  (let [unit (armies unit-id)]
                                    [(effective-power unit)
                                     (:initiative unit)]))
                                (keys armies)))


         targets {}]
    (if (empty? attacker-ids)
      targets

      (let [attacker-id (first attacker-ids)
            defender-ids (for [defender-id (keys armies)
                               :when (and (not (some #(= defender-id %) (vals targets)))
                                          (not= (:type (armies attacker-id))
                                                (:type (armies defender-id))))]
                           defender-id)
            chosen-defender  (last
                              (sort-by (juxt (fn [defender-id] (damage-delt (armies attacker-id)
                                                                            (armies defender-id)))
                                             (fn [defender-id] (effective-power (armies defender-id)))
                                             (fn [defender-id] (:initiative (armies defender-id))))
                                       defender-ids))]
        #_(println "DEFENDER" attacker-id :=> chosen-defender "::" (damage-delt (armies attacker-id)
                                                                              (armies chosen-defender)))

        (recur (rest attacker-ids)
               (assoc targets attacker-id chosen-defender))))))


(defn remove-units [armies unit-id n-lost]
  (let [unit (armies unit-id)]
    (if (> (:n unit) n-lost)
      (update-in armies [unit-id :n] - n-lost)
      (dissoc armies unit-id))))

(defn part1 [input]
  (loop [n 0
         armies (parse input)]
    #_(println "-----" n (keys armies) (map :n (map armies (keys armies))))
    (if (or (empty? (filter #(= :immune (:type %))
                            (vals armies)))
            (empty? (filter #(= :infection (:type %))
                            (vals armies))))
      (reduce + (map :n (vals armies)))
      (let [targets (select-targets armies)
            attack-order (reverse (sort-by #(:initiative (armies %)) (keys targets)))
            reduce-attack (fn [armies attacker-id]
                            (let [attacker (armies attacker-id)
                                  defender-id (targets attacker-id)
                                  defender (armies defender-id)]
                              (if (and attacker defender)
                                (let [damage (damage-delt attacker defender)
                                      n-lost (quot damage (:hp defender))]
                                  (remove-units armies defender-id n-lost))
                                armies)))]
        (recur (inc n)
               (reduce reduce-attack armies attack-order))))))


(defn p2-sim [unboosted boost]
  (let [boosted (reduce (fn [armies unit-id]
                          (let [unit (armies unit-id)]
                            (if (= :immune (:type unit))
                              (update-in armies [unit-id :attack] + boost)
                              armies)))
                        unboosted
                        (keys unboosted))]
    (loop [armies boosted]
      (cond
        (empty? (filter #(= :immune (:type %))
                        (vals armies)))
        nil

        (empty? (filter #(= :infection (:type %))
                        (vals armies)))
        (reduce + (map :n (vals armies)))

        :else
        (let [targets (select-targets armies)
              attack-order (reverse (sort-by #(:initiative (armies %)) (keys targets)))
              reduce-attack (fn [armies attacker-id]
                              (let [attacker (armies attacker-id)
                                    defender-id (targets attacker-id)
                                    defender (armies defender-id)]
                                (if (and attacker defender)
                                  (let [damage (damage-delt attacker defender)
                                        n-lost (quot damage (:hp defender))]
                                    (remove-units armies defender-id n-lost))
                                  armies)))]
          (let [next-armies (reduce reduce-attack armies attack-order)]
            (if (= armies next-armies)
              nil ;;TIE !
              (recur next-armies))))))))

(defn part2 [input]
  (let [armies (parse input)]
    (loop [boost 0]

      (if-let [res (p2-sim armies boost)]
        res
        (recur (inc boost))))))

