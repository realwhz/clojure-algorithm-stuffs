(ns clojure-algorithm-stuffs.sort)

(def ^:const quicksort-threshold 10)

(defn median-of-three
  "Get the median of three input"
  [x y z]
  (if (< x y)
    (if (< y z)
      y
      (if (< x z)
        z
        y))
    (if (< x z)
      x
      (if (< y z)
        z
        x))))

(defn pivot
  "Get the pivot using median-of-three strategy"
  [col]
  (let [n (count col)]
    (median-of-three (first col) (nth col (quot n 2)) (last col))))

(defn insert-into-sorted
  "Auxilliary function for insertion-sort"
  [sorted val]
  (loop [smallers {}
         biggers sorted]
    (if (empty? biggers)
      (concat smallers [val])
      (if (< val (first biggers))
        (concat smallers [val] biggers)
        (recur (concat smallers (vector (first biggers))) (next biggers))))))

(defn insertion-sort
  "Do insertion sort on given collection"
  [col]
  (reduce insert-into-sorted [] col))

(defn quicksort
  "Do quicksort on given collection"
  [col]
  (if (< (count col) quicksort-threshold)
    (insertion-sort col)
    (let [p (pivot col)
          smallers (filter #(> p %) col)
          biggers (next (filter #(<= p %) col))]
      (concat (quicksort smallers) [p] (quicksort biggers)))))
