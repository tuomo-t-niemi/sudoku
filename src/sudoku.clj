(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (let [[row col] coord]
    (get-in board [row col])))

(defn has-value? [board coord]
  (not (== 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[row col] coord]
    (reduce
      (fn [bag elim]
       (conj bag (get elim col)))
   #{}
   board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
      [row col]))

(defn block-top-left [[x y]]
  [(* 3 (quot x 3)) (* 3 (quot y 3))])

(defn block-coord-pairs [[x y]]
  (for [rr [x (+ x 1) (+ x 2)]
        cc [y (+ y 1) (+ y 2)]]
        [rr cc]))

(defn block-values [board coord]
  (let [bl-coor (block-coord-pairs (block-top-left coord))]
    (reduce
     (fn [bag ccr]
       (conj bag (value-at board ccr)))
     #{}
     bl-coor)))


(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [used-values
            (set/union
             (row-values board coord)
             (col-values board coord)
             (block-values board coord))]
        (set/difference all-values used-values))))


(defn containz-zero? [coll]
  (if (empty? coll)
    false
    (if (== 0 (first coll))
      true
      (containz-zero? (rest coll)))))

(defn filled? [board]
  (if (empty? board)
    true
    (if (containz-zero? (first board))
      false
      (filled? (rest board)))))

(defn rows [board]
  (reduce
   (fn [bag bb]
     (conj bag (set bb)))
   []
   board))

(defn valid-rows? [board]
  nil)

(defn row-coord-pairs []
  (for [col [0 1 2 3 4 5 6 7 8]]
        [0 col]))

(defn cols [board]
  (reduce
   (fn [bag coord]
     (conj bag (col-values board coord)))
   []
   (for [col [0 1 2 3 4 5 6 7 8]] [0 col])))

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  nil)

(defn valid-blocks? [board]
  nil)

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
