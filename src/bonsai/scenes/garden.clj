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


;;;; random cut/graft/rotate functions to show everything working.
;;;; @TODO: need to use mouse controls to select where to cut or graft.

(defn get-branches
  [{:keys [current-scene] :as state}]
  (filter (qpsprite/group-pred :branches) (get-in state [:scenes current-scene :sprites])))

(defn cut-random
  [{:keys [current-scene] :as state}]
  ;; @TODO: never cut the root node
  (let [branches (get-branches state)
        root (first (filter #(zero? (:L %)) branches))
        cutable (remove #(zero? (:L %)) branches)]
    (if (seq branches)
      (let [target (rand-nth branches)]
        (-> state
            (assoc-in [:scenes current-scene :sprites]
                      (conj (b/cut branches target)
                            root))
            (dissoc :latest-grafted-node)))
      state)))

(defn graft-random
  [{:keys [current-scene] :as state}]
  (let [branches (get-branches state)
        target (rand-nth branches)
        ;; @TODO: cleaner way of doing this
        new-branches (-> (b/create-tree
                          (last (:line target))
                          (* (:size target) 0.8)
                          ((rand-nth [+ -]) (:r target) 30)
                          (rand-nth [1 2 3]))
                         b/add-numbering
                         b/collapse)
        grafted (b/graft branches
                         target
                         new-branches)]
    (-> state
        (assoc-in [:scenes current-scene :sprites] grafted)
        ;; @TODO: this is kinda janky we need to find the latest node by looking at (inc L) of the target
        (assoc :latest-grafted-node (first (filter #(= (:L %) (inc (:L target))) grafted))))))

(defn rotate
  [state dr]
  (if-let [{:keys [L R] :as latest} (:latest-grafted-node state)]
    ;; @TODO: some nice wy of doing this to leverage the nested set abstraction a bit better.
    (qpsprite/update-sprites-by-pred
     state
     (fn [s] (<= L (:L s) (:R s) R))
     (fn [child]
       ;; Calculate the vector from the base of the latest node to the base of the child node, rotate the vector by `dr` and then update the child position
       (let [v (map - (:pos child) (:pos latest))
             rotated (qpu/rotate-vector v dr)
             new-pos (map + (:pos latest) rotated)]
         (-> child
             (assoc :pos new-pos)
             (update :r + dr)
             b/recalc-line))))
    state))

(defn handle-key-pressed
  [state e]
  (case (:key e)
    :c (cut-random state)
    :g (graft-random state)
    :d (rotate state 5)
    :a (rotate state -5)
    state))

(defn init
  "Initialise this scene"
  []
  {:sprites (sprites)
   :draw-fn draw-garden
   :update-fn update-garden
   :key-pressed-fns [handle-key-pressed]})
