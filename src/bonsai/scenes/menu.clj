(ns bonsai.scenes.menu
  (:require [quil.core :as q]
            [quip.sprite :as qpsprite]
            [quip.sprites.button :as qpbutton]
            [quip.scene :as qpscene]
            [quip.tween :as qptween]
            [quip.utils :as qpu]
            [bonsai.common :as c]))

(defn play
  "Transition from this scene to `:garden` with a 30 frame fade-out"
  [state]
  (qpscene/transition state
                      :garden
                      :transition-length 60
                      :transition-fn c/fade-to-white))

(def title-letter c/light-leaf-green)
(def title-digit c/blossom-pink-1)
(def subtitle c/light-leaf-green)

(defn blossom
  [pos]
  (qpsprite/animated-sprite :blossoms
                            pos
                            15 16
                            (rand-nth ["img/blossom-1.png"
                                       "img/blossom-2.png"
                                       "img/blossom-3.png"])
                            :vel [1 2]
                            :animations {:tumble {:frames 7
                                                  :y-offset 0
                                                  :frame-delay 5}}
                            :current-animation :tumble))

(defn add-sprite
  [{:keys [current-scene] :as state} sprite]
  (update-in state [:scenes current-scene :sprites] conj sprite))

(defn random-mouse-pos
  []
  (let [d 50
        mx (q/mouse-x)
        my (q/mouse-y)]
    [(+ mx (- (/ d 2) (rand-int d)))
     (+ my (- (/ d 2) (rand-int d)))]))

(defn sprites
  "The initial list of sprites for this scene"
  []
  (let [title-y-offset (* 0.25 (q/height))]
    [(qpsprite/text-sprite "b"
                           [(- (* 0.5 (q/width)) (* 3 (/ qpu/title-text-size 2)))
                            title-y-offset]
                           :color title-letter
                           :size qpu/title-text-size
                           :offsets [:left])
     (qpsprite/text-sprite "0"
                           [(- (* 0.5 (q/width)) (* 2 (/ qpu/title-text-size 2)))
                            title-y-offset]
                           :color title-digit
                           :size qpu/title-text-size
                           :offsets [:left])
     (qpsprite/text-sprite "nsa"
                           [(- (* 0.5 (q/width)) (* 1 (/ qpu/title-text-size 2)))
                            title-y-offset]
                           :color title-letter
                           :size qpu/title-text-size
                           :offsets [:left])
     (qpsprite/text-sprite "1"
                           [(+ (* 0.5 (q/width)) (* 2 (/ qpu/title-text-size 2)))
                            title-y-offset]
                           :color title-digit
                           :size qpu/title-text-size
                           :offsets [:left])

     (qpsprite/text-sprite "binary trees"
                           [(* 0.5 (q/width))
                            (+ title-y-offset (* 0.13 (q/height)))]
                           :color subtitle
                           :size 58)

     (qptween/tween-to-color
      (qpsprite/text-sprite "press <space> to start"
                            [(* 0.5 (q/width))
                             (* 0.8 (q/height))]
                            :color [255 255 255 255]
                            :size 30)
      [255 255 255 0]
      {:yoyo? true
       :repeat-times ##Inf})]))

(defn draw-menu
  "Called each frame, draws the current scene to the screen"
  [state]
  (qpu/background c/dark-green)
  (qpsprite/draw-scene-sprites state))

(defn add-mouse-blossoms
  [state]
  (if (and (not= (q/pmouse-x) (q/mouse-x))
           (not= (q/pmouse-y) (q/mouse-y))
           (zero? (mod (:global-frame state) 4)))
    (let [pos (random-mouse-pos)]
      (add-sprite state (blossom pos)))
    state))

(defn add-scene-blossoms
  [state]
  (if (zero? (mod (:global-frame state) 30))
    (let [pos [(rand-int (q/width)) -10]]
      (add-sprite state (blossom pos)))
    state))

(defn remove-old-blossoms
  [{:keys [current-scene] :as state}]
  (update-in state
             [:scenes current-scene :sprites]
             (fn [sprites]
               (keep
                (fn [s]
                  (if (and (= :blossoms (:sprite-group s))
                           (< (+ (q/height) 20) (second (:pos s))))
                    nil
                    s))
                sprites))))

(defn update-menu
  "Called each frame, update the sprites in the current scene"
  [state]
  (-> state
      add-mouse-blossoms
      add-scene-blossoms
      remove-old-blossoms
      qpsprite/update-scene-sprites
      qptween/update-sprite-tweens))

(defn handle-key
  [state e]
  (if (= :space (:key e))
    (play state)
    state))

;; @TODO: can we have the mouse push air currents through the falling blossoms instead of creating them?

(defn init
  "Initialise this scene"
  []
  {:sprites (sprites)
   :draw-fn draw-menu
   :update-fn update-menu
   :key-pressed-fns [handle-key]})
