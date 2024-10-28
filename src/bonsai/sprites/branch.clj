(ns bonsai.sprites.branch
  (:require [quip.utils :as qpu]
            [bonsai.common :as c]
            [quil.core :as q]))

;; we want to be able to reomve a branch, killing all its children recursively

;; we want to be able to modify the rotation of a branch

;; we want to be able to add a new child?

(defn random-tree
  []
  {:children []})

;; @TODO: docstring and maybe tidy up a bit for readability?
(defn add-numbering*
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
          children)
        ))))

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

(defn bend
  "Rotate a branch and all of its descendants by an angle 1`dr`.

  We calculate the vectors from the origin of the target branch to
  each descendant's branch, rotate these vectors and reposition the
  descendants according to the new end positions."
  [branches {:keys [pos r L R] :as target-node} dr]

  ;; @TODO: implement
  )

(defn draw-branch
  [{[p1 p2] :line size :size}]
  (qpu/stroke c/dark-slate-grey)
  (q/stroke-weight (/ size 6))
  (q/line p1 p2))

(defn branch
  [pos size r]
  {:sprite-group :branches
   :pos pos
   :size size
   :r r
   :line [pos (map + pos (map * (qpu/direction-vector r) (repeat size)))]
   :draw-fn draw-branch
   :update-fn identity})

;; @TODO: repeat of constructor line, should dedupe
(defn recalc-line
  [{:keys [pos r size] :as branch}]
  (assoc branch :line [pos (map + pos (map * (qpu/direction-vector r) (repeat size)))]))

;; @TODO: do we want to create the sprites in a tree structure so we have access to the parent at that point, then use the nested set for manipulating them in-game? [DONE] good idea, must add to docstring.
(defn create-tree
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
