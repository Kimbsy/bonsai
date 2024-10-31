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

(defn get-branches
  [{:keys [current-scene] :as state}]
  (filter (qpsprite/group-pred :branches) (get-in state [:scenes current-scene :sprites])))

(defn draw-highlighted-branch
  [{:keys [current-scene] :as state}]
  (let [branches (get-branches state)]
    (if-let [highlighted (first (filter :highlight? branches))]
      (b/draw-branch highlighted))))

;; @TODO: this is ugly and copy-pasta of the branch's own draw-fn, maybe that should just accept and override colour for highlights or being selected etc, then we could even have different highlight colours depending on the tool we're using
(defn draw-selected-branch
  [{:keys [current-scene] :as state}]
  (let [branches (get-branches state)]
    (if-let [target-coords (get-in state [:scenes current-scene :currently-selected-coords])]
      (if-let [{[p1 p2] :line :keys [size]} (b/get-from-coords branches target-coords)]
        (do
          (qpu/stroke c/acer-orange)
          (q/stroke-weight (/ size 6))
          (q/line p1 p2))))))

(defn draw-garden
  "Called each frame, draws the current scene to the screen"
  [state]
  (qpu/background c/sky-blue)
  (qpsprite/draw-scene-sprites state)

  ;; draw the highlighted branch again so it's on top
  (draw-highlighted-branch state)

  ;; draw the selected branch on top of that
  (draw-selected-branch state))

(defn highlight-closest
  "Highlight the closest branch to the current mouse pos if any are
  close enough."
  [{:keys [current-scene] :as state}]
  (let [branches (get-branches state)
        [closest distance] (b/get-closest-branch branches [(q/mouse-x) (q/mouse-y)])
        max-distance 50]
    (if (<= distance max-distance)
      (qpsprite/update-sprites-by-pred
       state
       (fn [{:keys [L R]}]
         (and (= L (:L closest))
              (= R (:R closest))))
       (fn [b]
         (assoc b :highlight? true)))
      state)))

(defn update-garden
  "Called each frame, update the sprites in the current scene"
  [state]
  (-> state
      qpsprite/update-scene-sprites
      highlight-closest))

(defn cut-highlighted
  "Remove the currently highlighted branch if any.

  Will not remove the root node of the tree."
  [{:keys [current-scene] :as state}]
  (let [branches (get-branches state)
        target (first (filter #(:highlight? %) branches))]
    (if (and target
             (not (zero? (:L target))))
      (-> state
          (assoc-in [:scenes current-scene :sprites]
                    (b/cut branches target)))
      state)))

(defn graft-highlighted
  "Graft a small random subtree onto the currently highlighted branch if
  any."
  [{:keys [current-scene] :as state}]
  (let [branches (get-branches state)]
    (if-let [target (first (filter #(:highlight? %) branches))]
      (let [;; @TODO: cleaner way of doing this
            new-branches (-> (b/create-tree
                              (last (:line target))
                              (* (:size target) 0.8)
                              ((rand-nth [+ -]) (:r target) (rand-int 60))
                              (rand-nth [1 2 3]))
                             b/add-numbering
                             b/collapse)
            grafted (b/graft branches
                             target
                             new-branches)]
        (assoc-in state [:scenes current-scene :sprites] grafted))
      state)))

(defn select-highlighted
  "Select the currently highlighted branch for bending if any."
  [{:keys [current-scene] :as state}]
  (let [branches (get-branches state)]
    (if-let [target (first (filter #(:highlight? %) branches))]
      (assoc-in state [:scenes current-scene :currently-selected-coords]
                (select-keys target [:L :R]))
      (assoc-in state [:scenes current-scene :currently-selected-coords]
                nil))))

(defn rotate-selected
  [{:keys [current-scene] :as state} dr]
  (if-let [target-coords (get-in state [:scenes current-scene :currently-selected-coords])]
    (let [branches (get-branches state)
          target (b/get-from-coords branches target-coords)]
      (assoc-in state [:scenes current-scene :sprites]
                (b/bend branches target dr)))
    state))

;; @TODO: need to have some UI icons for the different operations that we can click on

(defn handle-mouse-pressed
  [{:keys [current-scene] :as state} e]
  (let [current-tool (get-in state [:scenes current-scene :current-tool])]
    (case current-tool
      :cut (cut-highlighted state)
      :graft (graft-highlighted state)
      :bend (select-highlighted state)
      state)))

(defn handle-key-pressed
  [{:keys [current-scene] :as state} e]
  (case (:key e)
    :left (rotate-selected state -3)
    :right (rotate-selected state 3)
    :c (-> state
           (assoc-in [:scenes current-scene :current-tool] :cut)
           (assoc-in [:scenes current-scene :currently-selected-coords]
                     nil))
    :g (-> state
           (assoc-in [:scenes current-scene :current-tool] :graft)
           (assoc-in [:scenes current-scene :currently-selected-coords]
                     nil))
    :b (assoc-in state [:scenes current-scene :current-tool] :bend)
    state))

(defn init
  "Initialise this scene"
  []
  {:sprites (sprites)
   :draw-fn draw-garden
   :update-fn update-garden
   :mouse-pressed-fns [handle-mouse-pressed]
   :key-pressed-fns [handle-key-pressed]
   :current-tool :bend})
