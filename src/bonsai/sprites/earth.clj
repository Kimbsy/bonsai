(ns bonsai.sprites.earth
  (:require [quip.utils :as qpu]
            [bonsai.common :as c]
            [quil.core :as q]))

(defn draw-earth
  [depth]
  (let [w (q/width)
        h (q/height)
        d depth] 
    (q/begin-shape)
    (q/vertex 0 h)
    (q/vertex w h)
    (q/vertex w (- h d))
    (q/vertex 0 (- h d))
    (q/end-shape :close)))

(defn earth
  [height]
  {:sprite-group :earth
   :pos [(/ (q/width) 2) (* height (q/height))]
   :height height
   :draw-fn draw-earth
   :update-fn identity})
