(ns bonsai.sprites.branch
  (:require [quip.utils :as qpu]
            [bonsai.common :as c]
            [quil.core :as q]))

(defn add-numbering*
  "Take a tree-like data structure of branch sprites and add the
  appropriate `L` and `R` values to encode the structure with the
  nested set model.

  It's a recursive reduce which is a little opaque, but we essentially
  just keep track of the current value of a counter while doing a
  depth-first traversal of the tree, and conjing the updated children
  into a single result vector."
  [{:keys [children] :as tree} n]
  (let [{:keys [result-children next-n]}
        (reduce (fn [acc child]
                  (let [{:keys [result-tree next-n]} (add-numbering* child (:next-n acc))]
                    (-> acc
                        (update :result-children conj result-tree)
                        (assoc :next-n next-n))))
                {:result-children []
                 :next-n (inc n)}
                children)]
    {:result-tree (assoc tree
                         :children result-children
                         :L n
                         :R next-n)
     :next-n (inc next-n)}))

(defn add-numbering
  "A neater interface to `add-numbering*`."
  [tree]
  (:result-tree (add-numbering* tree 0)))

(defn collapse
  "Take the tree representation of the branches and collapse them down
  to a single sequence. We can remove the `:children` fro each as we
  will rely on the `L` and `R` values for structure."
  [tree]
  (map #(dissoc % :children)
       (tree-seq seq :children tree)))

(defn descendant-count
  "The number of descendants a node has can be determined directly from
  its `L` and `R` without counting them."
  [{:keys [L R]}]
  (/ (dec (- R L)) 2))

(defn all-descendants
  "The descendants of a node N are the nodes whose `L` and `R` are both
  between N's `L` and `R`"
  [branches {:keys [L R] :as parent}]
  (filter (fn [e]
            (<= L (:L e) (:R e) R))
          branches))

(defn childless?
  [{:keys [L R]}]
  (= L (dec R)))

(defn direct-children
  "Finding the direct (one level deeper) children of a nodeN is
  comparatively expensive.

  We know that the node with `L` one higher than N's is a direct
  child (the leftmost child in fact). We then recursively iterate
  through the other descendants looking for nodes with `L` one higher
  than this latest known child."
  [branches {:keys [L R] :as parent}]
  (if (childless? parent)
    []
    (let [leftmost-child (first (filter #(= (inc L) (:L %)) branches))]
      (loop [candidates (all-descendants branches parent)
             children [leftmost-child]
             latest-child leftmost-child]
        (if (seq candidates)
          (let [next-candidate (first candidates)]
            (if (= (inc (:R latest-child)) (:L next-candidate))
              (recur (rest candidates)
                     (conj children next-candidate)
                     next-candidate)
              (recur (rest candidates)
                     children
                     latest-child)))
          children)))))

(defn parent
  "The parent of a node N is the node with the highest `L` of all nodes
  that have both `L` less than N's `L` and `R` greater than N's `R`"
  [branches {:keys [L R] :as child}]
  (->> branches
       (filter (fn [b]
                 (and (< (:L b) L)
                      (< R (:R b)))))
       (sort-by :L)
       last))

(defn group-by-depth
  "By starting at the root node we can find all the direct children,
  then we can find all the direct children of these, adding a new
  group each iteration until one group of children are all childless.

  This uses multiple calls to `direct-children` and is probably quite
  expensive as a result. Probably best to limit it to scene
  initialization rather than anything that happens every frame."
  [branches]
  (let [root (first (filter #(= 0 (:L %)) branches))]
    (loop [groups [[root]]]
      (let [children (mapcat #(direct-children branches %) (last groups))]
        (if (seq children)
          (recur (conj groups children))
          groups)))))

(defn get-from-coords
  "Find the node in the branches which matches the `L` and `R` values."
  [branches {:keys [L R]}]
  (first (filter (fn [b] (and (= L (:L b))
                              (= R (:R b))))
                 branches)))

;; @TODO: would be cool to have a nice slashing cut animation at the base of the target node
(defn cut
  "To remove a subtree at node N we need to remove all nodes which have
  `L` and `R` between N's `L` and `R`.

   All the remaining nodes with `L` and/or `R` values greater than N's
  original `R` value need to be decremented to account for the nodes
  under N (and N itself) leaving the tree."
  [branches {:keys [L R] :as cut-node}]
  (let [remaining (filter (fn [b]
                            (not (<= L (:L b) (:R b) R)))
                          branches)
        
        adjustment (+ 2 (* 2 (descendant-count cut-node)))]
    (map (fn [b]
           ;; @TODO: this is ugly as heck, need to optionally update both `:L` and/or `:R`
           (if (< R (:R b))
             (update 
              (if (< R (:L b))
                (update b :L - adjustment)
                b)
              :R - adjustment)
             b))
         remaining)))

;; @TODO: could have a nice green/pink leaf/blossom burst at the target node when we graft
;; @TODO: would also be nice to have the branches grow with a tweened animation.
(defn graft
  "Add new branches to an existing node.

  New branches are supplied as a branches seq, we need to increment
  their `L` and `R` values based on the target node's `L` value, we
  need to increment later `L` and `R` values in the original seq
  relative to the number of new nodes we're adding."
  [branches {:keys [L R] :as target-node} new-branches]
  (let [adjustment (* 2 (count new-branches))]
    (concat (map (fn [b]
                   ;; @TODO: this is ugly as heck, need to optionally update both `:L` and/or `:R`
                   (if (< L (:R b))
                     (update 
                      (if (< L (:L b))
                        (update b :L + adjustment)
                        b)
                      :R + adjustment)
                     b))
                 branches)
            (map (fn [b]
                   (-> b
                       (update :L + (inc L))
                       (update :R + (inc L))))
                 new-branches))))

(defn branch-line
  "Calculate the line for a branch."
  ([{:keys [pos r size]}]
   (branch-line pos r size))
  ([pos r size]
   [pos (map + pos (map * (qpu/direction-vector r) (repeat size)))]))

(defn midpoint
  "Calculate the midpoint of a line."
  [line]
  (apply map (fn [a b] (/ (+ a b) 2)) line))

(defn bend
  "Rotate a branch and all of its descendants by an angle `dr`.

  We calculate the vectors from the origin of the target branch to
  each descendant's branch, rotate these vectors and reposition the
  descendants according to the new end positions."
  [branches {:keys [pos L R] :as target-node} dr]
  ;; @TODO: might want this to be a bit more of a generic "update-all-descendants" abstraction?
  (map (fn [branch]
         (if (<= L (:L branch) (:R branch) R)
           (let [v (map - (:pos branch) pos)
                 rotated (qpu/rotate-vector v dr)
                 new-pos (map + pos rotated)
                 ;; need to update `pos` and `r` before recalculating the `line` and `mp`
                 updated-branch (-> branch
                                    (update :r + dr)
                                    (assoc :pos new-pos))
                 line (branch-line updated-branch)]
             (-> updated-branch
                 (assoc :line line)
                 (assoc :mp (midpoint line))))
           branch))
       branches))

(defn draw-branch
  [{[p1 p2] :line :keys [size color hl-color highlight?]}]
  (if highlight?
    (qpu/stroke hl-color)
    (qpu/stroke color))
  (q/stroke-weight (/ size 6))
  (q/line p1 p2))

(defn branch
  [pos size r]
  (let [line (branch-line pos r size)]
    {:sprite-group :branches
     :pos pos
     :size size
     :color c/dark-slate-grey
     :hl-color c/ceramic-white
     :highlight? false
     :r r
     :line line
     :mp (midpoint line)
     :draw-fn draw-branch
     :update-fn (fn [b] (assoc b :highlight? false))}))

(defn create-tree
  "We create our tree (whole tree, or subtree if grafting) with a nested
  structure as it makes it easy to ensure the positions of all the
  children are correct. We'll store the sprites in a flat vector using
  our nested set model for ease of manipulation in-game."
  [pos size r depth]
  (let [b (branch pos size r)]
    (assoc b
           :children (if (pos? depth)
                       (let [dr 30]
                         [(create-tree (last (:line b))
                                       (* size 0.8)
                                       (+ r dr)
                                       (dec depth))
                          (create-tree (last (:line b))
                                       (* size 0.8)
                                       (- r dr)
                                       (dec depth))])
                       []))))

(defn get-closest-branch
  "Determine the branch with the closest midpoint to the specified
  `pos`.

  Returns a vector of the branch and it's distance."
  [branches [x y :as pos]]
  (->> branches
       (map (fn [b] [b (qpu/magnitude (map - (:mp b) pos))]))
       (sort-by second)
       first))
