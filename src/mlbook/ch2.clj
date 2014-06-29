(ns mlbook.ch2
  (:require [incanter.charts :refer [scatter-plot add-lines]]
            [incanter.core :refer [view]]
            [incanter.stats :refer [linear-model]]
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
