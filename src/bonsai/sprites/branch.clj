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

(def example
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

;; @TODO: thought, instead of fully reconstructing the tree so we can re-number after cutting a subtree, surely we can just take 2n away from L and R of all nodes with L < removed-L where n is the number of nodes in the subtree? Can write a test that asserts that cuting a sutree and shuffling nums is the same as re-numbering the tree?

;; @TODO: think about how position comes into this, just set and forget? what about when bending a branch with deep children? we need to know the parent's position and child's relative position. at leats needs thinkging about.

(defn random-tree
  []
  example)

;; @TODO not working
(defn add-numbering
  ([tree]
   (add-numbering tree (atom 0)))
  ([{:keys [children] :as tree} n]
   (if (empty? children)
     (let [L @n
           R (swap! n inc)]
       (swap! n inc)
       (assoc tree
              :L L
              :R L))
     (let [L @n]
       (swap! n inc)
       (assoc tree
              :L L
              :R L)))))

(defn flat
  [tree]
  (map #(dissoc % :children)
       (tree-seq seq :children tree)))

(defn all-descendants
  [branches {:keys [L R] :as parent}]
  (filter (fn [e]
            (<= L (:L e) (:R e) R))
          branches))

(defn child-count
  [{:keys [L R]}]
  (/ (dec (- R L)) 2))

(defn childless?
  [{:keys [L R]}]
  (= L (dec R)))



(comment

  ;; how to create a list of sprites representing a tree
  
  (let [t (random-tree)
        branches-data (-> t
                          add-numbering
                          flat)]
    (map branch branches-data)))
