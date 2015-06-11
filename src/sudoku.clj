(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

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
  nil)

(defn filled? [board]
  nil)

(defn rows [board]
  nil)

(defn valid-rows? [board]
  nil)

(defn cols [board]
  nil)

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
