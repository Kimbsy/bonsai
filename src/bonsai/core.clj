(ns bonsai.core
  (:gen-class)
  (:require [quip.core :as qp]
            [bonsai.scenes.menu :as menu]
            [bonsai.scenes.garden :as garden]))

(defn setup
  "The initial state of the game"
  []
  {})

(defn init-scenes
  "Map of scenes in the game"
  []
  {:menu   (menu/init)
   :garden (garden/init)})

;; Configure the game
(def bonsai-game
  (qp/game {:title          "bonsai"
            :size           [800 600]
            :setup          setup
            :init-scenes-fn init-scenes
            :current-scene  :menu}))

(defn -main
  "Run the game"
  [& args]
  (qp/run bonsai-game))
