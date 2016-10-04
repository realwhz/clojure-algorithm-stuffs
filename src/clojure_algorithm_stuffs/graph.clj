;; This is a lisp style graph modeling
;; It is based on ``How to Design Programs'' http://www.htdp.org/
;; For example,
;; (def Graph 
;;   '((A (B E))
;;     (B (E F))
;;     (C (D))
;;     (D ())
;;     (E (C F))
;;     (F (D G))
;;     (G ())))

(ns clojure-algorithm-stuffs.graph)

(defn neighbors
  "Find neighbors of n in graph g"
  [g n]
  (if (empty? g)
    ()
    (if (= (first (first g)) n)
      (second (first g))
      (neighbors (next g) n))))

(declare find-route-neighbor)

(defn find-route
  "Find a route from origination to destination in graph g"
  [g origination destination]
  (if (= origination destination)
    (list destination)
    (let [possible-route 
          (find-route-neighbor g (neighbors g origination) destination)]
      (if (empty? possible-route)
        ()
        (conj possible-route origination)))))

(defn find-route-neighbor
  "Find a route from neighbors to destination in graph g"
  [g neighbors destination]
  (if (empty? neighbors)
    ()
    (let [possible-route (find-route g (first neighbors) destination)]
      (if (empty? possible-route)
        (find-route-neighbor g (next neighbors) destination)
        possible-route))))
