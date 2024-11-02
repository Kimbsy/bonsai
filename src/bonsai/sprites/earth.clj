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

(defn draw-rabbit
  [{:keys [size] :as s}]
  (q/no-stroke)
  (q/fill c/dark-ceramic-white)
  (q/ellipse 300 500 (* s 30) (* s 20)))

(defn update-rabbit
  [r]
  )

(defn ->rabbit
  []
  {:sprite-group :rabbit
   :pos          [(/ (q/width) 2) ((- (q/height 30)))]
   :hop-rate     1
   :size         1
   :update-fn    update-rabbit
   :draw-fn      draw-rabbit})
