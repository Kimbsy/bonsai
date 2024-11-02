(ns bonsai.scenes.end
  (:require [bonsai.common :as c]
            [bonsai.sprites.branch :as b]
            [quil.core :as q]
            [quip.delay :as qpdelay]
            [quip.sprite :as qpsprite]
            [quip.utils :as qpu]
            [quip.scene :as qpscene]
            [quip.collision :as qpcollision]
            [quip.tween :as qptween]))

(defn change-season
  [{:keys [current-scene] :as state}]
  (let [next-seasons (get-in state [:scenes current-scene :next-seasons])]
    (-> state
        (qpsprite/update-sprites-by-pred
         (qpsprite/group-pred :branches)
         (fn [b]
           (assoc b :season (first next-seasons))))
        (assoc-in [:scenes current-scene :current-season] (first next-seasons))
        (update-in [:scenes current-scene :next-seasons] rest))))

(defn draw-end
  "Called each frame, draws the current scene to the screen"
  [{:keys [current-scene] :as state}]
  (qpu/background c/sky-blue)
  (qpsprite/draw-scene-sprites-by-layers state [:earth :clouds :text :branches :ui])

  ;; highlight selected tool
  (let [current-tool (get-in state [:scenes current-scene :current-tool])]
    (when-let [{[x y] :pos
                :keys [w h]
                :as ui-sprite} (first (filter #(= current-tool (:sprite-group %))
                                              (get-in state [:scenes current-scene :sprites])))]
      (q/stroke-weight 4)
      (qpu/stroke c/blossom-pink-1)
      (q/no-fill)
      (q/rect (- x (/ w 2)) (- y (/ h 2)) w h)))

  ;;draw foliage on top of the branches
  (let [branches (c/get-branches state)
        groups (b/group-by-depth branches)
        non-foliage-Ls (into #{} (map :L (apply concat (take 3 groups))))]
    (doall
     (->> branches
          (filter (fn [b]
                    (not (non-foliage-Ls (:L b)))))
          (map b/draw-foliage)))))

(defn update-end
  "Called each frame, update the sprites in the current scene"
  [state]
  (-> state
      qpsprite/update-scene-sprites
      qpdelay/update-delays
      qptween/update-sprite-tweens))

(defn cycle-season-delay
  []
  (qpdelay/delay
    60
    (fn [state]
      (-> state
          change-season
          (qpdelay/add-delay (cycle-season-delay))))))

(def sparse-haikus
  [["Sparse branches whisper,"
    "Reaching through stillness, they speak—"
    "A quiet strength breathes."]
   ["Few branches remain,"
    "Aged roots in a shallow bed,"
    "Graceful strength holds on."]
   ["Bare limbs stretch skyward,"
    "Spaces speak of silent years,"
    "Patience shapes each curve."]
   ["Empty limbs unfold,"
    "Air fills where green life once thrived,"
    "Beauty in restraint."]
   ["Wind through sparse branches,"
    "Echoes of quiet seasons,"
    "Whispers in the calm."]
   ["Sparse, yet unbroken,"
    "Bonsai clings to silent stone,"
    "Grace shaped in absence."]])

(def busy-haikus
  [["Branches dance and weave,"
    "Bursting with wild, leafy life—"
    "A tangled story."]
   ["Branches twist and crowd,"
    "Lively whispers interlace,"
    "Chaos full of life."]
   ["A canopy wild,"
    "Branches stretching everywhere,"
    "Nature’s busy art."]
   ["Layers thick with green,"
    "Branches endlessly entwined,"
    "Life in full embrace."]
   ["Every limb alive,"
    "Branching paths in constant play,"
    "Vibrant, lush, and full."]
   ["Leaves and twigs abound,"
    "Woven branches overhead,"
    "A maze of rich growth."]])

(def normal-haikus
  [["Strong roots, branches wide,"
    "Shade and rustling leaves at rest—"
    "Peaceful, steadfast tree."]
   ["Graceful branches sway,"
    "Leaves whisper in gentle winds,"
    "Calm beneath the sky."]
   ["Standing tall and still,"
    "Green arms reaching for the sun,"
    "Life flows through each limb."]
   ["Roots deep, branches high,"
    "Sheltering all with its grace,"
    "Nature’s gentle guard."]
   ["Sunlight filters through,"
    "Dappled patterns dance below,"
    "A quiet embrace."]
   ["Seasons come and go,"
    "Leaves bloom, fall, then bloom again—"
    "The tree stands patient."]])

(defn unchanged?
  [state branches]
  (let [pertinent (juxt :L :R :r)]
    (= (map pertinent branches)
       (map pertinent
            (get-in state [:scenes :garden :initial-branches])))))

(defn choose-poem
  [state branches]
  (cond
    (unchanged? state branches) ["you should change something"
                                 "it's not hard, it's kinda fun"
                                 "come on, have a go."]
    (< (count branches) 4) (rand-nth sparse-haikus)
    (< 130 (count branches)) (rand-nth busy-haikus)
    :else (rand-nth normal-haikus)))

(defn fade-in
  [{:keys [color] :as s}]
  (-> s
      (assoc :color (conj color 0))
      (qptween/add-tween
       (qptween/tween :color
                      255
                      :update-fn (fn [c d] (update c 3 + d))
                      :step-count 20))))

(defn display-poem-delay
  []
  (qpdelay/delay
    0
    (fn [{:keys [current-scene] :as state}]
      (let [branches (c/get-branches state)
            [l1 l2 l3] (choose-poem state branches)]
        (-> state
            (qpdelay/add-delay
             (qpdelay/add-sprites-to-scene
              30
              [(fade-in
                (qpsprite/text-sprite
                 l1
                 [(/ (q/width) 2) (* (q/height) 0.08)]
                 :color c/dark-green))]))
            (qpdelay/add-delay
             (qpdelay/add-sprites-to-scene
              60
              [(fade-in
                (qpsprite/text-sprite
                 l2
                 [(/ (q/width) 2) (* (q/height) 0.16)]
                 :color c/dark-green))]))
            (qpdelay/add-delay
             (qpdelay/add-sprites-to-scene
              90
              [(fade-in
                (qpsprite/text-sprite
                 l3
                 [(/ (q/width) 2) (* (q/height) 0.24)]
                 :color c/dark-green))])))))))

(defn display-exit-icon-delay
  []
  (qpdelay/add-sprites-to-scene
   100
   [(let [s 50]
      (assoc (qpsprite/image-sprite :finish
                                    [(- (q/width) s)
                                     (* 0.92 (q/height))]
                                    s s
                                    "img/finish-icon.png")
             :click-fn (fn [state]
                         ;; @TODO: reset scenes!!!!!
                         (q/no-stroke)
                         (qpscene/transition
                          state
                          :menu
                          :transition-length 60
                          :transition-fn c/fade-to-white))))]))

(defn handle-mouse-pressed
  [{:keys [current-scene] :as state} e]
  (let [ui-sprite-groups #{:finish}
        ui-sprites (filter #(ui-sprite-groups (:sprite-group %))
                           (get-in state [:scenes current-scene :sprites]))]
    (reduce (fn [acc-state ui-sprite]
              (if (qpcollision/pos-in-rect? {:pos [(:x e) (:y e)]} ui-sprite)
                ((:click-fn ui-sprite) acc-state)
                acc-state))
            state
            ui-sprites)))

(defn init
  []
  {:sprites []
   :draw-fn draw-end
   :update-fn update-end
   :mouse-pressed-fns [handle-mouse-pressed]
   :current-season :spring
   :next-seasons (cycle [:summer :autumn :winter :spring])
   :delays [(cycle-season-delay)
            (display-poem-delay)
            (display-exit-icon-delay)]})
