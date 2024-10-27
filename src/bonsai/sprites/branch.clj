(ns bonsai.sprites.branch
  (:require [quip.utils :as qpu]
            [bonsai.common :as c]
            [quil.core :as q]))

(defn draw-branch
  [{[p1 p2] :line children :children}]
  (qpu/stroke c/brown)
  (q/stroke-weight 3)
  (q/line p1 p2)
  (doseq [child children]
    (draw-branch child)))

(defn branch
  [pos size r depth max-depth]
  {:sprite-group :branches
   :pos pos
   :size size
   :r r
   :line [pos (map + pos (map * (qpu/direction-vector r) (repeat size)))]
   :children [(branch )]
   :draw-fn draw-branch
   :update-fn identity})



;; we want to be able to reomve a branch, killing all its children recursively

;; we want to be able to modify the rotation of a branch

;; we want to be able to add a new child?

;; at the start we want to make a random tree, then save it in a form tht is quick to draw and possible ot modify

;; we also want each branch to be a sprite (maybe?) for collision detection with mouse clicks

;; we want the branches to be in a tree strucure so we can easily kill sections

;; but maybe we also want it in a flat structure for collision detection? or maybe we should juts use some good tree walking abstractions? `tree-seq`? `walk`?

;; can we just keep a flat tructure and use left right numbering to let us remove subtrees? we'll need to be able to renumber the whole tree, but that shouldn't be impossible and only needs to happen once per remove?

(def raw-example
  {:children [{:children [{:children []}]}
              {:children [{:children []}
                          {:children []}]}]})

(def numbered-example
  {:L 0
   :R 11
   :children [{:L 1
               :R 4
               :children [{:L 2
                           :R 3
                           :children []}]}
              {:L 5
               :R 10
               :children [{:L 6
                           :R 7
                           :children []}
                          {:L 8
                           :R 9
                           :children []}]}]})

;; this lets us do nice things like intuitively know that a node is at the end (= r (inc l)) so we can put leaves or something there.

;; we an also store the ndoes in a flat vector (good for sprites) but still manage subtrees with simple filter queries (all children have l and r between parent's l and r values)

;; looks like this is called the nested set model and is used for representing trees in (e.g.) relational databases

;; so in order to use this we need to be able to take a tree and add L and R numbers `add-numbering`, we need to be able to flatten this tree into a seq `flatten`? `tree-seq`?, we need to be able to handle subtrees, so some kind of `get-all-descendants` sounds good, and we want to be able to count the number of descendants which we can do with (/ (dec (- r l)) 2)


;; @TODO: think about how position comes into this, just set and forget? what about when bending a branch with deep children? we need to know the parent's position and child's relative position. at leats needs thinkging about.








(defn random-tree
  []
  raw-example)

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

(defn all-descendants
  "The descendants of a node N are the nodes whose `L` and `R` are both
  between N's `L` and `R`"
  [branches {:keys [L R] :as parent}]
  (filter (fn [e]
            (<= L (:L e) (:R e) R))
          branches))

(defn descendant-count
  "The number of descendants a node has can be determined directly from
  its `L` and `R` without counting them."
  [{:keys [L R]}]
  (/ (dec (- R L)) 2))

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

(defn childless?
  [{:keys [L R]}]
  (= L (dec R)))

;; @TODO: want a util function for grouping a flat list of descendants by depth?
(defn group-by-depth
  ;; @NOTE it kinda feels like this is only possible if we reconstruct the tree fully?
  [branches])

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

(defn graft
  "Add new branches to an existing node.

  New branches are supplied as a branches seq, we need to increment
  their `L` and `R` values based on the target node's `L` value, we
  need to increment later `L` and `R` values in the original seq
  relative to the number of new nodes we're adding."
  [branches {:keys [L R] :as target-node} new-branches]
  (let [adjustment (* 2 (count new-branches))]
    (concat (map (fn [b]
                   ;; @TODO: same ugly nonsense
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
