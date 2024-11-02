(ns bonsai.scenes.garden
  (:require [quip.collision :as qpcollision]
            [quip.sprite :as qpsprite]
            [quip.utils :as qpu]
            [bonsai.common :as c]
            [bonsai.sprites.branch :as b]
            [bonsai.sprites.earth :as e]
            [quil.core :as q]))

(defn sprites
  "The initial list of sprites for this scene"
  []
  (concat (-> (b/create-tree [(/ (q/width) 2) (* 0.8 (q/height))]
                             100
                             0
                             6
                             :spring)
              b/add-numbering
              b/collapse)

          ;; ui buttons
          (let [s 50]
            [(assoc (qpsprite/image-sprite :seasons
                                           [s
                                            (* 0.92 (q/height))]
                                           s s
                                           "img/seasons-icon.png")
                    :click-fn (fn [{:keys [current-scene] :as state}]
                                (let [next-seasons (get-in state [:scenes current-scene :next-seasons])]
                                     (-> state
                                         (qpsprite/update-sprites-by-pred
                                          (qpsprite/group-pred :branches)
                                          (fn [b]
                                            (assoc b :season (first next-seasons))))
                                         (assoc-in [:scenes current-scene :current-season] (first next-seasons))
                                         (update-in [:scenes current-scene :next-seasons] rest)))))
             (assoc (qpsprite/image-sprite :cut
                                           [(- (* 0.5 (q/width))
                                               (* s 1.5))
                                            (* 0.92 (q/height))]
                                           s s
                                           "img/cut-icon.png")
                    :click-fn (fn [{:keys [current-scene] :as state}]
                                (-> state
                                    (assoc-in [:scenes current-scene :current-tool] :cut)
                                    (assoc-in [:scenes current-scene :currently-selected-coords]
                                              nil))))
             (assoc (qpsprite/image-sprite :graft
                                           [(* 0.5 (q/width))
                                            (* 0.92 (q/height))]
                                           s s
                                           "img/graft-icon.png")
                    :click-fn (fn [{:keys [current-scene] :as state}]
                                (-> state
                                    (assoc-in [:scenes current-scene :current-tool] :graft)
                                    (assoc-in [:scenes current-scene :currently-selected-coords]
                                              nil))))
             (assoc (qpsprite/image-sprite :bend
                                           [(+ (* 0.5 (q/width))
                                               (* s 1.5))
                                            (* 0.92 (q/height))]
                                           s s
                                           "img/bend-icon.png")
                    :click-fn (fn [{:keys [current-scene] :as state}]
                                (assoc-in state [:scenes current-scene :current-tool] :bend)))
             (assoc (qpsprite/image-sprite :finish
                                           [(- (q/width) s)
                                            (* 0.92 (q/height))]
                                           s s
                                           "img/finish-icon.png")
                    ;; @TODO!!!!!!!: confirmation dialog and transition to end scene
                    :click-fn identity)])))

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
  [{:keys [current-scene] :as state}]
  (qpu/background c/sky-blue)
  (qpsprite/draw-scene-sprites-by-layers state [:branches :ui])
  (e/draw-earth 125)

  ;; highlight selected tool
  (let [current-tool (get-in state [:scenes current-scene :current-tool])]
    (when-let [{[x y] :pos
                :keys [w h]
                :as ui-sprite} (first (filter #(= current-tool (:sprite-group %))
                                              (get-in state [:scenes current-scene :sprites])))]
      (qpu/stroke c/dark-green)
      (q/no-fill)
      (q/rect (- x (/ w 2)) (- y (/ h 2)) w h)))

  ;;draw foliage on top of the branches
  (let [branches (get-branches state)
        groups (b/group-by-depth branches)
        non-foliage-Ls (into #{} (map :L (apply concat (take 3 groups))))]
    (doall
     (->> branches
          (filter (fn [b]
                    (not (non-foliage-Ls (:L b)))))
          (map b/draw-foliage))))

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
        non-branch-sprites (filter #(not (= :branches (:sprite-group %)))
                                   (get-in state [:scenes current-scene :sprites]))
        target (first (filter #(:highlight? %) branches))]
    (if (and target
             (not (zero? (:L target))))
      (-> state
          (assoc-in [:scenes current-scene :sprites]
                    (concat non-branch-sprites
                            (b/cut branches target))))
      state)))

(defn graft-highlighted
  "Graft a small random subtree onto the currently highlighted branch if
  any."
  [{:keys [current-scene] :as state}]
  (let [branches (get-branches state)
        non-branch-sprites (filter #(not (= :branches (:sprite-group %)))
                                   (get-in state [:scenes current-scene :sprites]))]
    (if-let [target (first (filter #(:highlight? %) branches))]
      (let [;; @TODO: cleaner way of doing this
            new-branches (-> (b/create-tree
                              (last (:line target))
                              (* (:size target) 0.8)
                              ((rand-nth [+ -]) (:r target) (rand-int 60))
                              (rand-nth [1 2 3])
                              (get-in state [:scenes current-scene :current-season]))
                             b/add-numbering
                             b/collapse)
            grafted (b/graft branches
                             target
                             new-branches)]
        (assoc-in state [:scenes current-scene :sprites]
                  (concat non-branch-sprites
                          grafted)))
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
          non-branch-sprites (filter #(not (= :branches (:sprite-group %)))
                                   (get-in state [:scenes current-scene :sprites]))
          target (b/get-from-coords branches target-coords)]
      (assoc-in state [:scenes current-scene :sprites]
                (concat non-branch-sprites
                        (b/bend branches target dr))))
    state))

(defn handle-mouse-pressed
  [{:keys [current-scene] :as state} e]
  (let [ui-sprite-groups #{:seasons :cut :graft :bend :finish}
        ui-sprites (filter #(ui-sprite-groups (:sprite-group %))
                           (get-in state [:scenes current-scene :sprites]))
        ui-handled-state (reduce (fn [acc-state ui-sprite]
                                   (if (qpcollision/pos-in-rect? {:pos [(:x e) (:y e)]} ui-sprite)
                                     ((:click-fn ui-sprite) acc-state)
                                     acc-state))
                                 state
                                 ui-sprites)]
    (let [current-tool (get-in ui-handled-state [:scenes current-scene :current-tool])]
      (case current-tool
        :cut (cut-highlighted ui-handled-state)
        :graft (graft-highlighted ui-handled-state)
        :bend (select-highlighted ui-handled-state)
        ui-handled-state))))

(defn handle-key-pressed
  [{:keys [current-scene] :as state} e]
  (case (:key e)
    :left (rotate-selected state -3)
    :right (rotate-selected state 3)
    :a (rotate-selected state -3)
    :d (rotate-selected state 3)
    state))

(defn init
  "Initialise this scene"
  []
  {:sprites (sprites)
   :draw-fn draw-garden
   :update-fn update-garden
   :mouse-pressed-fns [handle-mouse-pressed]
   :key-pressed-fns [handle-key-pressed]
   :current-tool :bend
   :current-season :spring
   :next-seasons (cycle [:summer :autumn :winter :spring])})
