(ns advent2018.day2)

(defn count-trues
  "count the number of true? items"
  [items]
  (count (filter true? items)))

(defn make-count
  "poorly named, return a vector of [two? three?] where
  two? is a boolean indicating if any character appears twice,
  three? is a boolean indicating if any character appears three times."
  [box-id]
  (let [letter-counts (frequencies box-id)
        counts-seen (set (vals letter-counts))]
    ;; first make a map of character to character count
    ;; then make a set of all the unique counts
    ;; we can check this map to see if any specific count was seen
    [(contains? counts-seen 2) (contains? counts-seen 3)]))

(defn checksum [box-ids]
  (let [check-counts (map make-count box-ids)]
    ;; a vector of [two? three?]
    ;; just count the trues in the first position (two?) and the second position (three?) and multiply
    (let [twos (count-trues (map first check-counts))
          threes (count-trues (map second check-counts))]
      (* twos threes))))

(defn part1 [input]
  (checksum input))

(defn remove-differing
  "remove items from list1 that don't exist in list2 at the same index."
  [list1 list2]
  (when (seq list1)
    (if (= (first list1)
           (first list2))
      (lazy-seq (cons (first list1)
                      (remove-differing (rest list1) (rest list2))))
      (remove-differing (rest list1) (rest list2)))))

(defn try-to-match [id1 id2]
  (let [differing-chars (count-trues (map not= id1 id2))]
    (when (= 1 differing-chars)
      (apply str (remove-differing id1 id2)))))


(defn part2 [input]
  (loop [[id-to-test & remaining] input]
    (when id-to-test
      (if-let [match (some #(try-to-match id-to-test %) remaining)]
        match
        (recur remaining)))))
