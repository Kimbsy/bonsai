(ns bonsai.sprites.cloud
  (:require [quip.utils :as qpu]
            [quip.sprite :as qpsprite]
            [bonsai.common :as c]
            [quil.core :as q]))

(defn draw-cloud
  [{[x y] :pos w :width h :height op :opacity}]
  (q/fill (conj c/ceramic-white op))
  (q/no-stroke)
  (q/ellipse x y w h))

(defn update-cloud
  [c]
  (let [speed (get c :speed 0.5)]
    (update c :pos (fn [[x y]] [(- x speed) y]))))

(defn cloud
  [pos]
  [{:sprite-group :clouds
    :pos pos
    :width 200
    :height 50
    :speed 0.1
    :opacity 96
    :draw-fn draw-cloud
    :update-fn update-cloud}])
