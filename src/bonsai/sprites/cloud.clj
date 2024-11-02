(ns bonsai.sprites.cloud
  (:require [quip.utils :as qpu]
            [quip.sprite :as qpsprite]
            [bonsai.common :as c]
            [quil.core :as q]))

(defn draw-cloud
  [{[x y] :pos w :width h :height op :opacity}]
  (q/no-stroke)
  (q/fill (conj c/ceramic-white op))
  (q/begin-shape)
  (q/arc x
         y
         (/ w 2)
         h
         q/PI
         (* 2 q/PI))
  (q/arc (+ x (* w 0.25))
         y
         (/ w 2)
         (* h 1.5)
         q/PI
         (* 2 q/PI))
  (q/end-shape :close))

(defn update-cloud
  [c]
  (let [speed  (get c :speed)
        [x y]  (get c :pos)
        width  (get c :width)
        height (get c :height)
        min   (* height 2)
        max   (- (q/height) 100)]
    (update c :pos (fn [[x y]]
                     (if (<= x (- 0 width))
                       [(+(q/width) width) (+ min (rand-int (- max min)))]
                       [(- x speed) y])))))

(defn cloud
  [pos]
  [{:sprite-group :clouds
    :pos pos
    :width 200
    :height 50
    :speed 0.5
    :opacity 96
    :draw-fn draw-cloud
    :update-fn update-cloud}])
