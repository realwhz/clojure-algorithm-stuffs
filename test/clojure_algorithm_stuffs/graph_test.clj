(ns clojure-algorithm-stuffs.graph-test
  (:require [clojure.test :refer :all]
            [clojure-algorithm-stuffs.graph :refer :all]))

(def Graph 
  '((A (B E))
    (B (E F))
    (C (D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))

(deftest a-test
  (testing "find-route"
    (is (= (find-route Graph 'A 'G) '(A B E F G)))))
