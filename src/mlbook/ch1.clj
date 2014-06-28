(ns ch1
  (:require [clojure.core.matrix :refer :all]
            [clatrix.core :as cl]
            [clojure.core.matrix.operators :as M]
            [incanter.charts :refer [xy-plot add-points]]
            [incanter.core :refer [view]]))

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

;; Generating Matrices

(defn square-mat' [n e]
  (let [repeater #(repeat n %)]
    (matrix (-> e repeater repeater))))

(defn square-mat
  [n e & {:keys [implementation]
          :or {implementation
               :persistent-vector}}]
  (let [repeater #(repeat n %)]
    (matrix implementation (-> e repeater repeater))))

(defn id-mat [n]
  (let [init (square-mat n 0 :implementation :clatrix)
        identity-f (fn [i j n]
                     (if (= i j) 1 n))]
    (cl/map-indexed identity-f init)))

(defn rand-square-mat [n]
  (matrix
   (repeatedly n #(map rand-int (repeat n 100)))))

(defn rand-square-clmat [n]
  (cl/map rand-int (square-mat n 100 :implementation :clatrix)))

(cl/rnorm 10 25 10 10)
(cl/rnorm 5)
(cl/rnorm 3 4)

(defn id-computed-mat [n]
  (compute-matrix [n n] #(if (= %1 %2) 1 0)))

(defn rand-computed-mat [n m]
  (compute-matrix [n m]
                  (fn [i j] (rand-int 100))))

(compute-matrix :clatrix [3 4]
                (fn [i j] (if (= i j) 1 0)))

;; Adding matrices

(def A (matrix [[0 1 2] [3 4 5]]))
(def B (matrix [[0 0 0] [0 0 0]]))
(M/== B A)

(def C (M/+ A B))
(M/== C A)

(defn mat-eq [A B]
  (and (= (count A) (count B))
       (reduce #(and %1 %2) (map = A B))))

(defn mat-add [A B]
  (mapv #(mapv + %1 %2) A B))

(defn mat-add
  ([A B]
     (mapv #(mapv + %1 %2) A B))
  ([A B & more]
     (let [M (concat [A B more])]
       (reduce mat-add M))))

(def X (concat [A B] [C A] ))
(reduce mat-add X)
X

;; Multiplying matrices
(def A (matrix [[1 2 3] [4 5 6]]))
(def B (matrix [[10 20] [20 30] [30 40]]))
(def C (matrix [[11 12] [13 14]]))
(def N 10)

(pm A)
(pm B)
(shape A)
(shape B)
(shape (mmul A B))
(mmul A B)
(mmul B C)
(mmul C A)

(defn time-mat-mul [A B]
  (time (mmul A B)))

(time-mat-mul (rand-square-mat 100) (rand-square-mat 100))
(time-mat-mul (rand-square-clmat 100) (rand-square-clmat 100))

(pm (scale A 10))

;; Transposing and inverting matrices
(def A (matrix [[1 2 3] [4 5 6]]))
(pm (transpose A))

(def A (cl/matrix [[2 0] [0 2]]))
(mmul (inverse A) A)

(def A (cl/matrix [[1 2] [3 4]]))
(inverse A)

(def A (cl/matrix [[-2 2 3] [-1 1 3] [2 0 -1]]))

(det A)

;; Interpolating using matrices
(defn lmatrix [n]
  (compute-matrix :clatrix [n (+ n 2)]
                  (fn [i j] ({0 -1 1 2 2 -1}
                            (- j i) 0))))

(pm (lmatrix 4))

(defn problem [n n-observed lambda]
  (let [i (shuffle (range n))]
    {:L (mmul (lmatrix n) lambda)
     :observed (take n-observed i)
     :hidden (drop n-observed i)
     :observed-values (matrix :clatrix
                              (repeatedly n-observed rand))}))

(defn solve
  [{:keys [L observed hidden observed-values]
    :as problem}]
  (let [nc (column-count L)
        nr (row-count L)
        L1 (cl/get L (range nr) hidden)
        L2 (cl/get L (range nr) observed)
        l11 (mmul (transpose L1) L1)
        l12 (mmul (transpose L1) L2)]
    (assoc problem :hidden-values
           (mmul -1 (inverse l11) l12 observed-values))))

(defn plot-points [s]
  (let [X (concat (:hidden s) (:observed s))
        Y (concat (:hidden-values s) (:observed-values s))]
    (view
     (add-points
      (xy-plot X Y) (:observed s) (:observed-values s)))))

(defn plot-rand-sample []
  (plot-points (solve (problem 150 10 30))))

#_(plot-rand-sample)
