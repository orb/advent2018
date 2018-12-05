(ns advent2018.day5)

(defn same-char?
  "are the two characters the same?"
  [c1 c2]
  (= (Character/toLowerCase c1)
     (Character/toLowerCase c2)))

(defn react? [unit1 unit2]
  "do the two units react?"
  (and unit1
       unit2
       (not= unit1 unit2)
       (same-char? unit1 unit2)))

(defn react-all
  "react the whole chain.
   skip is a the skip character (probably should filter out that character before this to keep
   the logic here better.
   looking back at this, a reduce method is probably actually simpler - maybe rewrite."
  [polymer skip]
  (loop [text polymer
         result (list)]
    (if (seq text)
      (let [current (first text)
            prev (peek result)]
        (cond
          (and skip (same-char? skip current))
          (recur (rest text)
                 result)

          (react? prev current)
          (recur (rest text)
                 (rest result))

          :else
          (recur (rest text)
                 (conj result current))))
      (apply str (reverse result)))))

(defn part1 [input]
  (count (react-all input nil)))

(defn a-to-z []
  ;; I missed the inc on my first pass, but thankfully \z was not
  ;; the smallest one
  (map char (range (int \a)
                   (inc (int \z)))))
(defn part2 [input]
  (let [counts (for [unit (a-to-z)]
                 (count (react-all input unit)))]
    (reduce min counts)))
