(ns bonsai.scenes.garden
  (:require [quip.sprite :as qpsprite]
            [quip.utils :as qpu]
            [bonsai.common :as c]
            [bonsai.sprites.branch :as b]
            [quil.core :as q]))

(defn sprites
  "The initial list of sprites for this scene"
  []
  (-> (b/create-tree [(/ (q/width) 2) (* 0.8 (q/height))]
                     100
                     0
                     6)
      b/add-numbering
      b/collapse))

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

;; @TODO: add keybindings to cut, graft and rotate nodes to demonstrate everything working.

(defn init
  "Initialise this scene"
  []
  {:sprites (sprites)
   :draw-fn draw-garden
   :update-fn update-garden})
