(ns ch1
  (:require [clojure.core.matrix :refer :all]
            [clatrix.core :as cl]))

;; representing matricies
(matrix [[0 1 2] [3 4 5]])
(matrix '((0 1 2) (3 4 5)))

(def A (matrix [[0 1 2] [3 4 5]]))
(pm A)

(matrix? A)

(def A2 (cl/matrix [[0 1 2] [3 4 5]]))
A2
(pm A2)

(matrix :persistent-vector [[1 2] [2 1]])
(matrix :clatrix [[1 2] [2 1]])

(cl/matrix [0 1])
(cl/matrix [[0 1]])

(matrix? A2)
(cl/clatrix? A2)

(count A2)
(row-count A2)
(column-count A2)

(cl/get A2 1 1)
(cl/get A2 3)

(cl/set A2 1 2 0)
(pm A2)

(cl/map-indexed
 (fn [i j m] (* m 2)) A2)
