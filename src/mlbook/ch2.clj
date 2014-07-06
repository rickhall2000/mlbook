(ns mlbook.ch2
  (:require [incanter.charts :refer [scatter-plot add-lines xy-plot]]
            [incanter.core :refer [view sel to-matrix bind-columns]]
            [incanter.stats :refer [linear-model]]
            [incanter.datasets :refer [get-dataset]]
            [clojure.core.matrix :refer :all]
            [clatrix.core :as cl]))

(def X (cl/matrix [8.401 14.475 13.396 12.127 5.044
                   8.339 15.692 17.108 9.253 12.029]))

(def Y (cl/matrix [-1.57 2.32 0.424 0.814 -2.3
                   0.01 1.954 2.296 -0.635 0.328]))

(def linear-samp-scatter
  (scatter-plot X Y))

(defn plot-scatter []
  (view linear-samp-scatter))

#_(plot-scatter)

(def samp-linear-model
  (linear-model Y X))

(defn plot-model []
     (view (add-lines linear-samp-scatter
                      X (:fitted samp-linear-model))))

#_(plot-model)

(:coefs samp-linear-model)
(:residuals samp-linear-model)
(:sse samp-linear-model)
(:r-square samp-linear-model)

;; Understanding gradient descent

(def gradient-descent-precision 0.001)
(defn gradient-descent [F' x-start step]
  (loop [x-old x-start]
    (let [x-new (- x-old
                   (* step (F' x-old)))
          dx (- x-new x-old)]
      (if (< dx gradient-descent-precision)
        x-new
        (recur x-new)))))

;; Multivariable linear regression
(def iris
  (to-matrix (get-dataset :iris)))

(def X (sel iris :cols (range 1 5)))
(def Y (sel iris :cols 0))

(def iris-linear-model
  (linear-model Y X))

(defn plot-iris-linear-model []
  (let [x (range -100 100)
        y (:fitted iris-linear-model)]
    (view (xy-plot x y :x-label "X"
                   :y-label "Y"))))

(plot-iris-linear-model)

;; Understanding OLS

(defn linear-model-ols
  [MX MY]
  (let [X (bind-columns (repeat (row-count
                                 MX) 1) MX)
        Xt (cl/matrix (transpose X))
        Xt-X (cl/* Xt X)]
    (cl/* (inverse Xt-X) Xt MY)))

(def ols-linear-model
  (linear-model-ols X Y))

(def ols-linear-model-coefs
  (cl/as-vec ols-linear-model))

(cl/as-vec ols-linear-model)

(:coefs iris-linear-model)

(every? #(< % 0.001)
          (map -
               ols-linear-model-coefs
               (:coefs iris-linear-model)))
