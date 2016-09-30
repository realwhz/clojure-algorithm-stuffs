(ns clojure-algorithm-stuffs.sort-test
  (:require [clojure.test :refer :all]
            [clojure-algorithm-stuffs.sort :refer :all]))

(deftest a-test
  (testing "median-of-three"
    (is (= (median-of-three 1 2 5) 2))
    (is (= (median-of-three 3 100 10) 10))
    (is (= (median-of-three 3 -1 2) 2)))
  (testing "pivot"
    (is (= (pivot (range 9)) 4))
    (is (= (pivot (range 10)) 5)))
  (testing "insert-into-sorted"
    (is (= (insert-into-sorted (range 3) 5) '(0 1 2 5))))
  (testing "insertion-sort"
    (is (= (insertion-sort [3 2 1]) [1 2 3])))
  (testing "quicksort"
    (is (= (quicksort (reverse (range 13))) (range 13)))))
