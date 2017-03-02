;;; Binary Search Tree (BST)
;;
;; A BST is represented as follows:
;; either an empty list or a list with three elemenets:
;; the value, the left sub-BST, the right sub-BST.
;; For example, the BST
;;                   1
;;                  / \
;;                 2   3
;; is (1 (2 () ()) (3 () ()))

(ns clojure-algorithm-stuffs.bst)

(defn bst-create-empty
  "constructor - an empty BST"
  []
  ())

(defn bst-create
  "constructor"
  [value left right]
  (list value left right))

(defn bst-empty?
  "Check if the given BST is empty"
  [BST]
  (empty? BST))

(defn bst-value
  "Obtain the value of the root"
  [BST]
  (first BST))

(defn bst-left
  "Obtain the left sub-BST"
  [BST]
  (first (next BST)))

(defn bst-right
  "Obtain the right sub-BST"
  [BST]
  (first (nnext BST)))

(defn bst-add
  "Return a new BST with the value added"
  [BST value]
  (cond
    (bst-empty? BST) (bst-create value (bst-create-empty) (bst-create-empty))
    (< value (bst-value BST)) (bst-create (bst-value BST) (bst-add (bst-left BST) value) (bst-right BST))
    (> value (bst-value BST)) (bst-create (bst-value BST) (bst-left BST) (bst-add (bst-right BST) value))
    :else BST))

(defn bst-leaf?
  "Check if the given BST has neither left nor right sub-BST"
  [BST]
  (and (bst-empty? (bst-left BST)) (bst-empty? (bst-right BST))))

(defn bst-leftmost-child
  "Return the left most child"
  [BST]
  (cond
    (bst-empty? BST) (bst-create-empty)
    (bst-empty? (bst-left BST)) BST
    :else (bst-leftmost-child (bst-left BST))))

(declare bst-rm)

(defn bst-rm-root
  "Return a new BST with root removed"
  [BST]
  (cond
    (bst-leaf? BST) (bst-create-empty)
    (bst-empty? (bst-left BST)) (bst-right BST)
    (bst-empty? (bst-right BST)) (bst-left BST)
    :else (let [replace-value (bst-value (bst-leftmost-child (bst-right BST)))
                new-right-tree (bst-rm (bst-right BST) replace-value)]
            (bst-create replace-value (bst-left BST) new-right-tree))))

(defn bst-rm
  "Return a new BST with the value removed"
  [BST value]
  (cond
    (bst-empty? BST) (bst-create-empty)
    (= value (bst-value BST)) (bst-rm-root BST)
    (< value (bst-value BST)) (bst-create (bst-value BST) (bst-rm (bst-left BST) value) (bst-right BST))
    :else (bst-create (bst-value BST) (bst-left BST) (bst-rm (bst-right BST) value))))

(defn bst-find?
  "Find if the value is present in the BST"
  [BST value]
  (cond
    (bst-empty? BST) false
    (= value (bst-value BST)) true
    (< value (bst-value BST)) (bst-find? (bst-left BST) value)
    :else (bst-find? (bst-right BST) value)))

(defn bst-traverse-inorder
  "In-order traverse of the BST"
  [BST]
  (cond
    (bst-empty? BST) '()
    :else (concat (bst-traverse-inorder (bst-left BST)) (list (bst-value BST)) (bst-traverse-inorder (bst-right BST)))))

(defn bst-size
  "Return the number of nodes in the BST"
  [BST]
  (cond
    (bst-empty? BST) 0
    :else (+ 1 (bst-size (bst-left BST) (bst-right BST)))))

(defn bst-height
  "Return the height of the BST"
  [BST]
  (cond
    (bst-empty? BST) -1
    :else (+ 1 (max (bst-height (bst-left BST)) (bst-height (bst-right BST))))))

(defn bst-balanced?
  "Check if the BST is balanced"
  [BST]
  (cond
    (bst-empty? BST) true
:else (and (= (bst-height (bst-left BST) (bst-height (bst-right BST)))) (bst-balanced? (bst-left BST)) (bst-balanced? (bst-right BST)))))

(defn bst-sort
  "Sort based on BST"
  [col]
  (bst-traverse-inorder (reduce bst-add () col)))
