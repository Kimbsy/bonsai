(ns bonsai.sprites.earth
  (:require [quip.utils :as qpu]
            [quip.sprite :as qpsprite]
            [bonsai.common :as c]
            [quil.core :as q]))

(defn draw-earth
  [depth]
  (let [w (q/width)
        h (q/height)
        d (- h depth)]
    (q/fill c/dark-green)
    (q/no-stroke)
    (q/begin-shape)
    (q/vertex 0 d)
    (q/bezier-vertex (/ w 2)
                     (- d 100)
                     (/ w 2)
                     (+ d 100)
                     w d)
    (q/vertex w h)
    (q/vertex 0 h)
    (q/end-shape :close)))
