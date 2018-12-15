(ns advent2018.day14)

;; Enjoyed this one.
;;
;; Some problems:

;; - I got distracted trying to optimize part2 using strings and even
;; a java ArrayList, but then thinking through the performance, but
;; vectors, used correctly, were enough
;; - 03121 is octal notation in clojure. I was gettting the wrong answer for a while because of this
;; - when you add two recipes, you've added two possible end conditions

;; ----------------------------------------
(defn new-recipes [n1 n2]
  (let [total (+ n1 n2)]
    (if (>= total 10)
      [1 (mod total 10)]
      [total])))

(defn part1 [target-n]
  (loop [recipes [3 7]
         n 0
         pos1 0
         pos2 1]
    (if (> n (+ 10 target-n))
      (apply str (->> recipes
                      (drop target-n)
                      (take 10)))
      (let [next-recipes
            (into recipes
                  (new-recipes (recipes pos1)
                               (recipes pos2)))]
        (recur next-recipes
               (inc n)
               (mod (+ pos1 1 (recipes pos1))
                    (count next-recipes))
               (mod (+ pos2 1 (recipes pos2))
                    (count next-recipes)))))))

;; ----------------------------------------

(defn ends-with
  "does list2 end with list2?"
  [list1 list2]
  (let [len1 (count list1)
        len2 (count list2)]
    (and (> len1 len2)
         (= list2 (subvec list1 (- len1 len2))))))

(defn str->digits
  "convert a string of digits into a vector of numbers"
  [input]
  (vec (map #(- (int %) (int \0)) input)))

(defn part2 [input]
  (let [target (str->digits input)]
    (loop [recipes [3 7]
           pos1 0
           pos2 1]
      (let [r1 (recipes pos1)
            r2 (recipes pos2)
            new-recipe (+ r1 r2)
            next-recipes (cond-> recipes
                           (>= new-recipe 10) (conj 1)
                           true (conj (mod new-recipe 10)))]

        ;; this is perhaps the trickiest part, checking end conditions when
        ;; we add both 1 and 2 recipes. I feel like this should be able to be
        ;; more concise here.
        (cond
          (ends-with next-recipes target)
          (- (count next-recipes) (count target))

          (and (>= new-recipe 10)
               (ends-with (pop next-recipes) target))
          (- (count next-recipes) 1 (count target))

          :else
          (recur next-recipes
                 (mod (+ pos1 1 r1) (count next-recipes))
                 (mod (+ pos2 1 r2) (count next-recipes))))))))
