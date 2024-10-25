(ns bonsai.scenes.garden
  (:require [quip.sprite :as qpsprite]
            [quip.utils :as qpu]
            [bonsai.common :as c]
            [bonsai.sprites.branch :as b]))

(defn sprites
  "The initial list of sprites for this scene"
  []
  [#_(b/branch [400 300] 50 20)])

(defn draw-garden
  "Called each frame, draws the current scene to the screen"
  [state]
  (qpu/background c/sky-blue)
  (qpsprite/draw-scene-sprites state))

(defn update-garden
  "Called each frame, update the sprites in the current scene"
  [state]
  (-> state
      qpsprite/update-scene-sprites))

(defn init
  "Initialise this scene"
  []
  {:sprites (sprites)
   :draw-fn draw-garden
   :update-fn update-garden})
