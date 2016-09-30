(ns clojure-algorithm-stuffs.bst-test
  (:require [clojure.test :refer :all]
            [clojure-algorithm-stuffs.bst :refer :all]))

(deftest a-test
  (testing "bst-create-empty/bst-create"
    (is (= (bst-create-empty) ()))
    (is (= (bst-create 1 () ()) '(1 () ()))))
  (testing "bst-add"
    (is (= (reduce bst-add () '(2 3 1)) '(2 (1 () ()) (3 () ())))))
  (testing "bst-traverse-inorder"
    (is (= (bst-traverse-inorder (reduce bst-add () '(2 3 1))) '(1 2 3)))))
